{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}


module Web.Fn.Monadic
  ( -- * Application Setup
    FnRequest
  , Fn(..)
  , defaultFnRequest
  , toWai
    -- * Routing
  , Req
  , Route
  , route
  , fallthrough
  , (==>)
  , (!=>)
  , (//)
  , path
  , end
  , anything
  , segment
  , method
  , Fn.FromParam(..)
  , Fn.ParamError(..)
  , param
  , paramDef
  , paramOpt
  , Fn.File(..)
  , file
  , files
  -- * Basic Responses
  , staticServe
  , sendFile
  , okText
  , okJson
  , okHtml
  , errText
  , errHtml
  , notFoundText
  , notFoundHtml
  , redirect
  , redirectReferer
  -- * Redirect Responses
  , redirect3xx
  , redirect301
  , redirect302
  , redirect303
  -- * Text Responses
  , text
  , text200
  , text403
  , text404
  , text410
  , text500
  , text503
  -- * HTML Responses
  , html
  , html200
  , html403
  , html404
  , html410
  , html500
  , html503
  -- * JSON Responses
  , json
  , json200
  , json403
  , json404
  , json410
  , json500
  , json503
  -- * Sreaming Responses
  , stream
  , streamFile
  -- * Helpers
  , Fn.tempFileBackEnd'
  ) where

--------------------------------------------------------------------------------
import qualified Blaze.ByteString.Builder.Char.Utf8 as Blaze
import           Control.Concurrent.MVar             ( MVar
                                                     , newMVar
                                                     , tryTakeMVar
                                                     , modifyMVar_
                                                     )
import           Control.Monad                       ( join )
import           Control.Monad.IO.Class              ( MonadIO, liftIO )
import           Control.Monad.Trans.Resource        ( InternalState
                                                     , closeInternalState
                                                     , createInternalState
                                                     )
import           Data.Aeson                          ( ToJSON, encode )
import           Data.ByteString                     ( ByteString )
import           Data.ByteString.Builder             ( lazyByteString )
import           Data.Text                           ( Text )
import qualified Data.Text.Encoding                 as Text
import           Network.HTTP.Types                  ( Status
                                                     , StdMethod(..)
                                                     , ResponseHeaders
                                                     )
import qualified Network.HTTP.Types                 as Http
import           Network.Wai                         ( Request(..)
                                                     , Response
                                                     , StreamingBody
                                                     , Application
                                                     , responseBuilder
                                                     , responseStream
                                                     )
import           Network.Wai.Parse                   ( Param
                                                     , parseRequestBody
                                                     )
import qualified Network.Wai.Parse                  as Parse
import           Web.Fn                              ( FnRequest
                                                     , Req
                                                     , defaultFnRequest
                                                     )
import qualified Web.Fn                             as Fn
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Application setup


class MonadIO m => Fn m where
  getRequest :: m FnRequest
  setRequest :: FnRequest -> m a -> m a


-- | Convert an Fn application. Provide a function to run monad 'm' into 'IO',
-- and we'll create a WAI application by updating the 'FnRequest'
-- value for each call.

toWai :: Fn m => (m Response -> IO Response) -> m Response -> Application
toWai run app req cont = do
  mv <- newMVar Nothing
  resp <- run $ setRequest (req, Just mv) app
  posted <- tryTakeMVar mv
  case join posted of
    Nothing -> return ()
    Just (_,is) -> closeInternalState is
  cont resp


--------------------------------------------------------------------------------
-- Routing


-- | The type of a route, constructed with @pattern ==> handler@.

type Route m = Req -> m (Maybe (m (Maybe Response)))


-- | The main construct for Fn, 'route' takes a list of potential matches
-- (which, once they match, may still end up deciding not to handle the request
-- hence the double 'Maybe'). It can be nested.
--
-- @
-- app =
--   route
--     [ end ==> index
--     , path "foo" \/\/ path "bar" \/\/ segment \/? param "id" ==> h
--     ]
--
--   where
--     index :: Fn m => m (Maybe Response)
--     index =
--       okText "This is the index."
--
--     h :: Fn m => Text -> Text -> m (Maybe Response)
--     h s i =
--       okText ("got path \/foo\/" <> s <> ", with id=" <> i)
-- @

route :: Fn m => [Route m] -> m (Maybe Response)
route pths =
  do
    (r,post) <- getRequest

    let
      m = either (const GET) id (Http.parseMethod (requestMethod r))
      req = (r, filter (/= "") (pathInfo r), queryString r, m, post)

    route' req pths

  where
    route' _  [] = return Nothing
    route' req (x : xs) =
      x req >>=
       \case
         Nothing ->
          route' req xs

         Just action ->
           action >>=
            \case
              Nothing ->
                route' req xs

              Just resp ->
                return (Just resp)


-- | The 'route' function (and all your handlers) return
-- @m (Maybe Response)@, because each can elect to not respond (in
-- which case we will continue to match on routes). But to construct
-- an application, we need a response in the case that nothing matched
-- — this is what 'fallthrough' allows you to specify. In particular,
-- 'notFoundText' and 'notFoundHtml' may be useful.

fallthrough :: Monad m => m (Maybe Response) -> m Response -> m Response
fallthrough a ft = do
  a >>= maybe ft return


-- | The non-body parsing connective between route patterns and the
-- handler that will be called if the pattern matches. The type is not
-- particularly illuminating, as it uses polymorphism to be able to
-- match route patterns with varying numbers (and types) of parts with
-- functions of the corresponding number of arguments and types.

(==>) ::
  Fn m =>
  (Req -> m (Maybe (Req, k -> m (Maybe a)))) ->
  k ->
  Req ->
  m (Maybe (m (Maybe a)))
(match ==> handle) req = do
   rsp <- match req
   case rsp of
     Nothing ->
      return Nothing

     Just ((_,pathInfo',_,_,_), k) -> do
       (r, mv) <- getRequest
       return $ Just $ setRequest (r { pathInfo = pathInfo' }, mv) (k handle)


-- | The connective between route patterns and the handler that parses
-- the body, which allows post params to be extracted with 'param' and
-- allows 'file' to work (otherwise, it will trigger a runtime error).

(!=>) ::
  Fn m =>
  (Req -> m (Maybe (Req, k -> m (Maybe a)))) ->
  k ->
  Req ->
  m (Maybe (m (Maybe a)))
(match !=> handle) req = do
  getRequest >>=
    \case
      (_, Nothing) ->
        return Nothing

      (r, Just mv) -> do
        liftIO $ readBody mv r
        rsp <- match req
        case rsp of
          Nothing ->
            return Nothing

          Just ((_,pathInfo',_,_,_), k) -> do
            return $ Just $
              setRequest (r { pathInfo = pathInfo' }, Just mv) (k handle)


-- | Connects two path segments. Note that when normally used, the
-- type parameter r is 'Req'. It is more general here to facilitate
-- testing.

(//) ::
  Fn m =>
  (r -> m (Maybe (r, k -> k'))) ->
  (r -> m (Maybe (r, k' -> a))) ->
  r -> m (Maybe (r, k -> a))
(match1 // match2) req = do
  r1 <- match1 req
  case r1 of
    Nothing -> return Nothing
    Just (req', k) ->
      do r2 <- match2 req'
         return $ case r2 of
                    Nothing -> Nothing
                    Just (req'', k') -> Just (req'', k' . k)


-- | Matches a literal part of the path. If there is no path part
-- left, or the next part does not match, the whole match fails.

path :: Fn m => Text -> Req -> m (Maybe (Req, a -> a))
path s req =
  liftIO $ Fn.path s req


-- | Matches there being no parts of the path left. This is useful when
-- matching index routes.

end :: Fn m => Req -> m (Maybe (Req, a -> a))
end req =
  liftIO $ Fn.end req


-- | Matches anything.

anything :: Fn m => Req -> m (Maybe (Req, a -> a))
anything req =
  liftIO $ Fn.anything req


-- | Captures a part of the path. It will parse the part into the type
-- specified by the handler it is matched to. If there is no segment, or
-- if the segment cannot be parsed as such, it won't match.

segment :: (Fn m, Fn.FromParam p) => Req -> m (Maybe (Req, (p -> a) -> a))
segment req =
  liftIO $ Fn.segment req


-- | Matches on a particular HTTP method.

method :: Fn m => StdMethod -> Req -> m (Maybe (Req, a -> a))
method m r =
  liftIO $ Fn.method m r


-- | Matches on a query parameter of the given name. It is parsed into
-- the type needed by the handler, which can be a 'Maybe' type if the
-- parameter is optional, or a list type if there can be many. If the
-- parameters cannot be parsed into the type needed by the handler, it
-- won't match.
--
-- Note: If you have used the '!=>' connective, so that the request
-- body has been parsed, this will also match post parameters (and
-- will combine the two together). If you haven't used that connective
-- (so the pattern is matched to handler with '==>'), it will only
-- match query parameters.

param :: (Fn m, Fn.FromParam p) => Text -> Req -> m (Maybe (Req, (p -> a) -> a))
param n req =
  liftIO $ Fn.param n req


-- | Like 'param' but provides a default value to the handler if the parameter
-- is not present or cannot be parsed into the type expected by the handler.

paramDef :: (Fn m, Fn.FromParam p) => Text -> p -> Req -> m (Maybe (Req, (p -> a) -> a))
paramDef n def req =
  param n req >>=
    \case
      Nothing -> return $ Just (req, \k -> k def)
      Just a  -> return $ Just a


-- | If the specified parameters are present, they will be parsed into the
-- type needed by the handler, but if they aren't present or cannot be
-- parsed, the handler will still be called.
--
-- Note: If you have used the '!=>' connective, so that the request
-- body has been parsed, this will also match post parameters (and
-- will combine the two together). If you haven't used that connective
-- (so the pattern is matched to handler with '==>'), it will only
-- match query parameters.

paramOpt :: (Fn m, Fn.FromParam p) => Text -> Req -> m (Maybe (Req, (Either Fn.ParamError p -> a) -> a))
paramOpt n req =
  liftIO $ Fn.paramOpt n req


-- | Matches an uploaded file with the given parameter name.

file :: Fn m => Text -> Req -> m (Maybe (Req, (Fn.File -> a) -> a))
file n req =
  liftIO $ Fn.file n req


-- | Matches all uploaded files, passing their parameter names and
-- contents.

files :: Fn m => Req -> m (Maybe (Req, ([(Text, Fn.File)] -> a) -> a))
files req =
  liftIO $ Fn.files req


--------------------------------------------------------------------------------
-- Basic Responses


-- | Serves static files out of the specified path according to the
-- request path. Note that if you have matched parts of the path,
-- those will not be included in the path used to find the static
-- file. For example, if you have a file @static\/img\/a.png@, and do:
--
-- > path "img" ==> staticServe "static"
--
-- It will match @img\/img\/a.png@, not @img\/a.png@. If you wanted that,
-- you could:
--
-- > anything ==> staticServe "static"
--
-- If no file is found, or if the path has @..@ or starts with @/@,
-- this will continue routing.

staticServe :: Fn m => Text -> m (Maybe Response)
staticServe d =
  getRequest >>= liftIO . Fn.staticServe d


-- | Sends a specific file specified by path. It will specify the
-- content-type if it can figure it out by the file extension.
--
-- If no file exists at the given path, it will keep routing.

sendFile :: FilePath -> IO (Maybe Response)
sendFile =
  liftIO . Fn.sendFile


-- | Returns 'Text' as a plain text response (@text/plain@)
-- with 200 status code.

okText :: Fn m => Text -> m (Maybe Response)
okText =
  liftIO . Fn.okText


-- | Returns 'Text' as a JSON response (@application/json@)
-- with 200 status code.

okJson :: Fn m => Text -> m (Maybe Response)
okJson =
  liftIO . Fn.okJson


-- | Returns Html (in 'Text') as a response (@text/html@)
-- with 200 status code.

okHtml :: Fn m => Text -> m (Maybe Response)
okHtml =
  liftIO . Fn.okHtml


-- | Returns 'Text' as a plain text response (@text/plain@)
-- with a 500 status code.

errText :: Fn m => Text -> m (Maybe Response)
errText =
  liftIO . Fn.errText


-- | Returns Html (in 'Text') as a response (@text/html@)
-- with 500 status code.

errHtml :: Fn m => Text -> m (Maybe Response)
errHtml =
  liftIO . Fn.errHtml


-- | Returns a 404 with the given 'Text' as a body. Note that this
-- returns a @m Response@ not an @m (Maybe Response)@ because the
-- expectation is that you are calling this with 'fallthrough'.

notFoundText :: Fn m => Text -> m Response
notFoundText =
  liftIO . Fn.notFoundText


-- | Returns a 404 with the given html as a body. Note that this
-- returns a @m Response@ not an @m (Maybe Response)@ because the
-- expectation is that you are calling this with 'fallthrough'.

notFoundHtml :: Fn m => Text -> m Response
notFoundHtml =
  liftIO . Fn.notFoundHtml


-- | Redirects to the given url with 303 status code (See Other).
-- Note that the target is not validated, so it should be an absolute path/url.

redirect :: Fn m => Text -> m (Maybe Response)
redirect =
  liftIO . Fn.redirect


-- | Redirects to the referrer, if present in headers, else to "/".

redirectReferer :: Fn m => m (Maybe Response)
redirectReferer =
  getRequest >>= liftIO . Fn.redirectReferer


--------------------------------------------------------------------------------
-- Redirect Responses


-- | Returns a redirect response with the given status code and url.
-- Note that the target is not validated, so it should be an absolute path/url.

redirect3xx :: Fn m => Status -> Text -> m (Maybe Response)
redirect3xx status target =
  return . Just
    $ responseBuilder status [(Http.hLocation, Text.encodeUtf8 target)]
    $ Blaze.fromText ""


-- | Redirects to the given url with 301 status code (Moved Permanently).
-- Note that the target is not validated, so it should be an absolute path/url.

redirect301 :: Fn m => Text -> m (Maybe Response)
redirect301 target =
  redirect3xx Http.status301 target


-- | Redirects to the given url with 302 status code (Found).
-- Note that the target is not validated, so it should be an absolute path/url.

redirect302 :: Fn m => Text -> m (Maybe Response)
redirect302 target =
  redirect3xx Http.status302 target


-- | Redirects to the given url with 303 status code (See Other).
-- Note that the target is not validated, so it should be an absolute path/url.

redirect303 :: Fn m => Text -> m (Maybe Response)
redirect303 =
  redirect


--------------------------------------------------------------------------------
-- Text Responses


-- | Returns a response with the given status code, mime type, and 'Text' body.

text :: Fn m => Status -> ByteString -> Text -> m (Maybe Response)
text status content body =
  return $ Just $
    responseBuilder status [(Http.hContentType, content)] (Blaze.fromText body)


-- | Returns a 200 (OK) response with the given mime type and 'Text' body.

text200 :: Fn m => ByteString -> Text -> m (Maybe Response)
text200 content body =
  text Http.status200 content body


-- | Returns a 403 (Forbidden) response with the given mime type
-- and 'Text' body.

text403 :: Fn m => ByteString -> Text -> m (Maybe Response)
text403 content body =
  text Http.status403 content body


-- | Returns a 404 (Not Found) response with the given mime type
-- and 'Text' body.

text404 :: Fn m => ByteString -> Text -> m (Maybe Response)
text404 content body =
  text Http.status404 content body


-- | Returns a 410 (Gone) response with the given mime type and 'Text' body.

text410 :: Fn m => ByteString -> Text -> m (Maybe Response)
text410 content body =
  text Http.status410 content body


-- | Returns a 500 (Internal Server Error) response with the given mime type
-- and 'Text' body.

text500 :: Fn m => ByteString -> Text -> m (Maybe Response)
text500 content body =
  text Http.status500 content body


-- | Returns a 503 (Service Unavailable) response with the given mime type
-- and 'Text' body.

text503 :: Fn m => ByteString -> Text -> m (Maybe Response)
text503 content body =
  text Http.status503 content body


--------------------------------------------------------------------------------
-- HTML Responses


-- | Returns a @text/html@ response with the given 'Status' code
-- and a response body with the given 'Text' as HTML.

html :: Fn m => Status -> Text -> m (Maybe Response)
html status body =
  text status "text/html; charset=utf-8" body


-- | Returns a response with a 200 (OK) status code
-- and a response body with the given 'Text' as HTML.

html200 :: Fn m => Text -> m (Maybe Response)
html200 =
  html Http.status200


-- | Returns a response with a 403 (Forbidden) status code
-- and a response body with the given 'Text' as HTML.

html403 :: Fn m => Text -> m (Maybe Response)
html403 =
  html Http.status403


-- | Returns a response with a 404 (Not Found) status code
-- and a response body with the given 'Text' as HTML.

html404 :: Fn m => Text -> m (Maybe Response)
html404 =
  html Http.status404


-- | Returns a response with a 410 (Gone) status code
-- and a response body with the given 'Text' as HTML.

html410 :: Fn m => Text -> m (Maybe Response)
html410 =
  html Http.status410


-- | Returns a response with a 500 (Internal Server Error) status code
-- and a response body with the given 'Text' as HTML.

html500 :: Fn m => Text -> m (Maybe Response)
html500 =
  html Http.status500


-- | Returns a response with a 503 (Service Unavailable) status code
-- and a response body with the given 'Text' as HTML.

html503 :: Fn m => Text -> m (Maybe Response)
html503 =
  html Http.status503


--------------------------------------------------------------------------------
-- JSON Responses


-- | Returns a @application/json@ response with the given 'Status' code
-- and a response body with the JSON encoding of the given value 'a'.

json :: (Fn m, ToJSON a) => Status -> a -> m (Maybe Response)
json status val =
  return
    $ Just
    $ responseBuilder status [(Http.hContentType, "application/json; charset=utf-8")]
    $ lazyByteString
    $ encode val


-- | Returns a response with a 200 (OK) status code
-- and a response body with the JSON encoding of the given value 'a'.

json200 :: (Fn m, ToJSON a) => a -> m (Maybe Response)
json200 =
  json Http.status200


-- | Returns a response with a 403 (Forbidden) status code
-- and a response body with the JSON encoding of the given value 'a'.

json403 :: (Fn m, ToJSON a) => a -> m (Maybe Response)
json403 =
  json Http.status403


-- | Returns a response with a 404 (Not Found) status code
-- and a response body with the JSON encoding of the given value 'a'.

json404 :: (Fn m, ToJSON a) => a -> m (Maybe Response)
json404 =
  json Http.status404


-- | Returns a response with a 410 (Gone) status code
-- and a response body with the JSON encoding of the given value 'a'.

json410 :: (Fn m, ToJSON a) => a -> m (Maybe Response)
json410 =
  json Http.status410


-- | Returns a response with a 500 (Internal Service Error) status code
-- and a response body with the JSON encoding of the given value 'a'.

json500 :: (Fn m, ToJSON a) => a -> m (Maybe Response)
json500 =
  json Http.status500


-- | Returns a response with a 503 (Service Unavailable) status code
-- and a response body with the JSON encoding of the given value 'a'.

json503 :: (Fn m, ToJSON a) => a -> m (Maybe Response)
json503 =
  json Http.status503


--------------------------------------------------------------------------------
-- Streaming Responses


-- | Returns a streaming response with the given 'ResponseHeaders'
-- and 'StreamingBody'.

stream :: Fn m => ResponseHeaders -> StreamingBody -> m (Maybe Response)
stream headers body =
  return . Just $ responseStream Http.status200 headers body


-- | Returns a streaming response with the given file name and 'StreamingBody'.

streamFile :: Fn m => ByteString -> StreamingBody -> m (Maybe Response)
streamFile filename body =
  stream
    [ ( "Content-Disposition"
      , "attachment; filename=\"" <> filename <> "\""
      )
    ]
    body


--------------------------------------------------------------------------------


readBody :: MVar (Maybe (([Param], [Parse.File FilePath]), InternalState)) -> Request -> IO ()
readBody mv req =
  modifyMVar_ mv $
    \r ->
      case r of
        Nothing -> do
          is <- createInternalState
          rb <- parseRequestBody (Fn.tempFileBackEnd' is) req
          return (Just (rb, is))

        Just _ ->
          return r
