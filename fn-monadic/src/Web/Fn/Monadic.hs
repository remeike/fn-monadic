{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}


{-|

This package provides a simple framework for routing and responses.
It exposes two different interfaces: an IO-based interface that can be
found in Web.Fn and the more generic, monad-based interface found
in this module.

The two primary goals of the monadic interface are:

1. All web handler functions work with instances of the Fn typeclass.
This makes the library easily compatible with many different types of
application architectures.

2. Web handlers are functions with typed parameters. When routing, we
specify many parameters (most commonly, numeric ids, but can be many
things), so the handlers should be functions that take those as
parameters.
-}


module Web.Fn.Monadic
  ( -- * Application Setup
    FnRequest
  , FnResponse
  , Fn(..)
  , Internal.defaultFnRequest
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
  , FromParam(..)
  , ParamError(..)
  , param
  , paramBool
  , paramMany
  , paramDef
  , paramOpt
  , File(..)
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
  , notFoundJson
  , redirect
  , redirectReferer
  , skip
    -- * Utility Functions for Requests
  , waiRequest
  , waiRequestHeaders
  , lookupRequestHeader
  , getParams
  , lookupParam
  , decodeJsonBody
  ) where

--------------------------------------------------------------------------------
import           Control.Arrow                 ( second )
import           Control.Concurrent.MVar       ( newMVar, tryTakeMVar )
import           Control.Monad                 ( join )
import           Control.Monad.IO.Class        ( MonadIO, liftIO )
import           Control.Monad.Reader          ( ReaderT(..)
                                               , ask
                                               , local
                                               )
import           Control.Monad.Trans.Resource  ( closeInternalState )
import           Data.Aeson                    ( FromJSON, ToJSON )
import qualified Data.Aeson                   as Json
import           Data.ByteString               ( ByteString )
import qualified Data.CaseInsensitive         as CI
import           Data.Either                   ( lefts, rights )
import           Data.Text                     ( Text )
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as Text
import           Data.Text.Read                ( decimal, double )
import           Network.HTTP.Types            ( StdMethod(..)
                                               , RequestHeaders
                                               , Query
                                               )
import qualified Network.HTTP.Types           as Http
import qualified Network.Mime                 as Mime
import           Network.Wai                   ( Request(..)
                                               , Response
                                               , Application
                                               , responseFile
                                               , strictRequestBody
                                               )
import           Network.Wai.Parse             ( Param )
import           System.Directory              ( doesFileExist )
import           System.FilePath               ( takeExtension )
--------------------------------------------------------------------------------
import           Web.Fn.Internal               ( PostMVar, File(..), FnRequest )
import qualified Web.Fn.Internal              as Internal
import           Web.Fn.Response               ( FnResponse
                                               , redirect303
                                               , html200
                                               , json200
                                               , html500
                                               )
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Application setup


-- | The Fn monad.

class MonadIO m => Fn m where
  getRequest :: m FnRequest
  setRequest :: FnRequest -> m a -> m a


instance Internal.RequestContext r => Fn (ReaderT r IO) where
  getRequest = fmap Internal.getRequest ask
  setRequest req = local (\ctxt -> Internal.setRequest ctxt req)


-- | Convert an Fn application. Provide a function to run monad 'Fn m' into 'IO',
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


-- | The parts of the path, when split on /, and the query.

type Req = (Request, [Text], Query, StdMethod, PostMVar)


-- | The type of a route, constructed with @pattern ==> handler@.

type Route m = Req -> m (Maybe (m FnResponse))


-- | The main construct for Fn, 'route' takes a list of potential matches
-- (which, once they match, may still end up deciding not to handle the request
-- hence the double 'Maybe'). It can be nested.
--
-- @
-- app =
--   route
--     [ end ==> index
--     , path "foo" \/\/ path "bar" \/\/ segment \/\/ param "id" ==> h
--     ]
--
--   where
--     index :: Fn m => m FnResponse
--     index =
--       okText "This is the index."
--
--     h :: Fn m => Text -> Text -> m FnResponse
--     h s i =
--       okText ("got path \/foo\/" <> s <> ", with id=" <> i)
-- @

route :: Fn m => [Route m] -> m FnResponse
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
-- @m FnResponse@, because each can elect to not respond (in
-- which case we will continue to match on routes). But to construct
-- an application, we need a response in the case that nothing matched
-- — this is what 'fallthrough' allows you to specify. In particular,
-- 'notFoundText' and 'notFoundHtml' may be useful.

fallthrough :: Monad m => m FnResponse -> m Response -> m Response
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
        liftIO $ Internal.readBody mv r
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
  Monad m =>
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

path :: Monad m => Text -> Req -> m (Maybe (Req, a -> a))
path s req =
  return $
    case req of
      (r,y:ys,q,m,x) | y == s -> Just ((r,ys, q, m, x), id)
      _                       -> Nothing


-- | Matches there being no parts of the path left. This is useful when
-- matching index routes.

end :: Monad m => Req -> m (Maybe (Req, a -> a))
end req =
  return $
    case req of
      (_,[],_,_,_) -> Just (req, id)
      _            -> Nothing


-- | Matches anything.

anything :: Monad m => Req -> m (Maybe (Req, a -> a))
anything req =
  return $ Just (req, id)


-- | Captures a part of the path. It will parse the part into the type
-- specified by the handler it is matched to. If there is no segment, or
-- if the segment cannot be parsed as such, it won't match.

segment :: (Monad m, FromParam p) => Req -> m (Maybe (Req, (p -> a) -> a))
segment req =
  return $
    case req of
      (r,y:ys,q,m,x) ->
        case fromParam [y] of
          Left _  -> Nothing
          Right p -> Just ((r, ys, q, m, x), \k -> k p)

      _ ->
        Nothing



-- | Matches on a particular HTTP method.

method :: Monad m => StdMethod -> Req -> m (Maybe (Req, a -> a))
method m r@(_,_,_,m',_) | m == m' = return $ Just (r, id)
method _ _                        = return Nothing


-- | Error returned when route parameter fails to parse into the expect type.

data ParamError
  = ParamMissing
  | ParamTooMany
  | ParamUnparsable
  | ParamOtherError Text
  deriving (Eq, Show)


-- | A class that is used for parsing for 'param' and 'paramOpt'.
-- and 'segment'.

class FromParam a where
  fromParam :: [Text] -> Either ParamError a


instance FromParam Text where
  fromParam [x] = Right x
  fromParam []  = Left ParamMissing
  fromParam _   = Left ParamTooMany


instance FromParam Int where
  fromParam [t] =
    case decimal t of
      Left _ ->
        Left ParamUnparsable

      Right m | snd m /= "" ->
        Left ParamUnparsable

      Right (v, _) ->
        Right v

  fromParam [] = Left ParamMissing
  fromParam _  = Left ParamTooMany


instance FromParam Double where
  fromParam [t] =
    case double t of
      Left _ ->
        Left ParamUnparsable

      Right m | snd m /= "" ->
        Left ParamUnparsable

      Right (v, _) ->
        Right v

  fromParam [] = Left ParamMissing
  fromParam _  = Left ParamTooMany


instance FromParam a => FromParam [a] where
  fromParam params =
    let
      res = fmap (fromParam . (:[])) params
    in
    case lefts res of
      [] -> Right $ rights res
      _  -> Left $ ParamOtherError "Couldn't parse all parameters."


instance FromParam a => FromParam (Maybe a) where
  fromParam [x] = Just <$> fromParam [x]
  fromParam []  = Right Nothing
  fromParam _   = Left ParamTooMany


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

param ::
  (MonadIO m, FromParam p) => Text -> Req -> m (Maybe (Req, (p -> a) -> a))
param n req@(_,_,q,_,mv) = do
  ps <- liftIO (Internal.getMVarParams mv)
  return $
    case findParamMatches n (q ++ map (second Just) ps) of
      Right y -> Just (req, \k -> k y)
      Left _  -> Nothing


-- | Checks if route has parameter. It will match whether the parameter is
-- present or not, passing a 'Bool' value to the handler. If the parameter is
-- not present, it will pass 'False' to the handler. If the parameter
-- is present, even if it doesn't have a value (e.g @/foo?bar@ or @/foo?bar=@),
-- it will pass 'True' to the handler.

paramBool :: MonadIO m => Text -> Req -> m (Maybe (Req, (Bool -> a) -> a))
paramBool n req@(_,_,q,_,mv) = do
  ps <- liftIO (Internal.getMVarParams mv)
  return $
    case lookup (Text.encodeUtf8 n) (q ++ map (second Just) ps) of
      Just _  -> Just (req, \k -> k True)
      Nothing -> Just (req, \k -> k False)


{-# DEPRECATED paramMany "Use 'param' with a list type, or define param parsing for non-empty list." #-}
-- | Matches on query parameters of the given name. If there are no
-- parameters, or they cannot be parsed into the type needed by the
-- handler, it won't match.

paramMany :: (MonadIO m, FromParam p) => Text -> Req -> m (Maybe (Req, ([p] -> a) -> a))
paramMany n req@(_,_,q,_,mv) = do
  ps <- liftIO (Internal.getMVarParams mv)
  return $
    case findParamMatches n (q ++ map (second Just) ps) of
      Left _   -> Nothing
      Right ys -> Just (req, \k -> k ys)


-- | Like 'param' but provides a default value to the handler if the parameter
-- is not present or cannot be parsed into the type expected by the handler.

paramDef ::
  (MonadIO m, FromParam p) => Text -> p -> Req -> m (Maybe (Req, (p -> a) -> a))
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

paramOpt ::
  (MonadIO m, FromParam p) =>
  Text -> Req -> m (Maybe (Req, (Either ParamError p -> a) -> a))
paramOpt n req@(_,_,q,_,mv) = do
  ps <- liftIO (Internal.getMVarParams mv)
  return $ Just (req, \k -> k (findParamMatches n (q ++ map (second Just) ps)))


-- | Matches an uploaded file with the given parameter name.

file :: MonadIO m => Text -> Req -> m (Maybe (Req, (File -> a) -> a))
file n req@(r,_,_,_,mv) = do
  fs <- liftIO (Internal.getMVarFiles mv r)
  return $
    case filter ((== n) . fst) fs of
      [(_, f)] -> Just (req, \k -> k f)
      _        -> Nothing


-- | Matches all uploaded files, passing their parameter names and
-- contents.

files :: MonadIO m => Req -> m (Maybe (Req, ([(Text, File)] -> a) -> a))
files req@(r,_,_,_,mv) = do
  fs <- liftIO (Internal.getMVarFiles mv r)
  return $ Just (req, \k -> k fs)


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

staticServe :: Fn m => Text -> m FnResponse
staticServe directory = do
  req <- fmap fst getRequest
  let pth = Text.intercalate "/" (directory : pathInfo req)
  if "/" `Text.isPrefixOf` pth || ".." `Text.isInfixOf` pth then
    return Nothing
  else
    sendFile (Text.unpack pth)


-- | Sends a specific file specified by path. It will specify the
-- content-type if it can figure it out by the file extension.
--
-- If no file exists at the given path, it will keep routing.

sendFile :: MonadIO m => FilePath -> m FnResponse
sendFile pth = do
  exists <- liftIO (doesFileExist pth)
  if exists then
    let
      contentType =
        [ ( Http.hContentType
          , Mime.defaultMimeLookup (Text.pack (takeExtension pth))
          )
        ]
    in
    return $ Just $ responseFile Http.status200 contentType pth Nothing
  else
    return Nothing


-- | Returns 'Text' as a plain text response (@text/plain@)
-- with 200 status code.

okText :: Monad m => Text -> m FnResponse
okText body =
  fmap Just $ Internal.waiPlainText Http.status200 body



-- | Returns 'Text' as a JSON response (@application/json@)
-- with 200 status code.

okJson :: Monad m => Text -> m FnResponse
okJson =
  json200


-- | Returns Html (in 'Text') as a response (@text/html@)
-- with 200 status code.

okHtml :: Monad m => Text -> m FnResponse
okHtml =
  html200


-- | Returns 'Text' as a plain text response (@text/plain@)
-- with a 500 status code.

errText :: Monad m => Text -> m FnResponse
errText body =
  fmap Just $ Internal.waiPlainText Http.status500 body


-- | Returns Html (in 'Text') as a response (@text/html@)
-- with 500 status code.

errHtml :: Monad m => Text -> m FnResponse
errHtml =
  html500


-- | Returns a 404 with the given 'Text' as a body. Note that this
-- returns a @m Response@ not an @m FnResponse@ because the
-- expectation is that you are calling this with 'fallthrough'.

notFoundText :: Monad m => Text -> m Response
notFoundText body =
  Internal.waiPlainText Http.status404 body


-- | Returns a 404 with the given html as a body. Note that this
-- returns a @m Response@ not an @m FnResponse@ because the
-- expectation is that you are calling this with 'fallthrough'.

notFoundHtml :: Monad m => Text -> m Response
notFoundHtml body =
  Internal.waiHtml Http.status404 body


-- | Returns a 404 with the given html as a body. Note that this
-- returns a @m Response@ not an @m FnResponse@ because the
-- expectation is that you are calling this with 'fallthrough'.

notFoundJson :: (Monad m, ToJSON a) => a -> m Response
notFoundJson val =
  Internal.waiJson Http.status404 val


-- | Redirects to the given url with 303 status code (See Other).
-- Note that the target is not validated, so it should be an absolute path/url.

redirect :: Monad m => Text -> m FnResponse
redirect =
  redirect303


-- | Redirects to the referrer, if present in headers, else to "/".

redirectReferer :: Fn m => m FnResponse
redirectReferer = do
  headers <- fmap (requestHeaders . fst) getRequest
  case lookup Http.hReferer headers of
    Nothing      -> redirect "/"
    Just referer -> redirect (Text.decodeUtf8 referer)


-- | Skip over the current route, causing the Fn application to continue
-- attempting to match on any routes that might follow.

skip :: Monad m => m FnResponse
skip =
  return Nothing


--------------------------------------------------------------------------------
-- Utility Functions for Requests


-- | Returns the WAI 'Request' directly.

waiRequest :: Fn m => m Request
waiRequest =
  fmap fst getRequest


-- | Returns the WAI 'RequestHeaders' directly.

waiRequestHeaders :: Fn m => m RequestHeaders
waiRequestHeaders =
  fmap requestHeaders waiRequest


-- | Returns the value for the given request header key, if it exists.

lookupRequestHeader :: Fn m => Text -> m (Maybe Text)
lookupRequestHeader name = do
  headers <- waiRequestHeaders
  return $ fmap Text.decodeUtf8 $ lookup (CI.mk (Text.encodeUtf8 name)) headers


-- | Returns all of the query params in an 'FnRequest'

getParams :: Fn m => m [Param]
getParams = do
  (_, postMVar) <- getRequest
  liftIO (Internal.getMVarParams postMVar)


-- | Returns the value for the given query parameter, parsed into the expected
-- type.

lookupParam :: (Fn m, FromParam p) => Text -> m (Maybe p)
lookupParam name = do
  params <- getParams
  case findParamMatches name (fmap (second Just) params) of
    Right p -> return (Just p)
    Left  _ -> return Nothing


-- | Decode the JSON request body into the expected value.

decodeJsonBody :: (Fn m, FromJSON a) => m (Either Text a)
decodeJsonBody = do
  body <- waiRequest >>= liftIO . strictRequestBody
  case Json.eitherDecode body of
    Left err    -> return (Left $ Text.pack err)
    Right value -> return (Right value)



--------------------------------------------------------------------------------
-- Helpers


findParamMatches ::
  FromParam p =>
  Text -> [(ByteString, Maybe ByteString)] -> Either ParamError p
findParamMatches name params =
  fromParam
    $ map (maybe "" (Text.decodeUtf8With (\_ _ -> Just '\65533')) . snd)
    $ filter ((== Text.encodeUtf8 name) . fst)
    $ params
