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
  , subroute
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
  -- * Standard Responses
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
  -- * Text Responses
  , text
  , text200
  , text301
  , text302
  , text403
  , text404
  , text410
  , text500
  , text503
  -- * HTML Responses
  , html
  , html200
  , html301
  , html302
  , html403
  , html404
  , html410
  , html500
  , html503
  -- * JSON Responses
  , json
  , json200
  , json301
  , json302
  , json403
  , json404
  , json410
  , json500
  , json503
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


-- | Convert an Fn application (provide a function to run monad m into IO,
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


type Route m = Req -> m (Maybe (m (Maybe Response)))


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


subroute :: Fn m => [Route m] -> Route m
subroute =
  const . return . Just . route


fallthrough :: (Monad m) => m (Maybe Response) -> m Response -> m Response
fallthrough a ft = do
  a >>= maybe ft return


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



path :: Fn m => Text -> Req -> m (Maybe (Req, a -> a))
path s req =
  liftIO $ Fn.path s req


end :: Fn m => Req -> m (Maybe (Req, a -> a))
end req =
  liftIO $ Fn.end req


anything :: Fn m => Req -> m (Maybe (Req, a -> a))
anything req =
  liftIO $ Fn.anything req


segment :: (Fn m, Fn.FromParam p) => Req -> m (Maybe (Req, (p -> a) -> a))
segment req =
  liftIO $ Fn.segment req


method :: Fn m => StdMethod -> Req -> m (Maybe (Req, a -> a))
method m r =
  liftIO $ Fn.method m r


param :: (Fn m, Fn.FromParam p) => Text -> Req -> m (Maybe (Req, (p -> a) -> a))
param n req =
  liftIO $ Fn.param n req


paramDef :: (Fn m, Fn.FromParam p) => Text -> p -> Req -> m (Maybe (Req, (p -> a) -> a))
paramDef n def req =
  param n req >>=
    \case
      Nothing -> return $ Just (req, \k -> k def)
      Just a  -> return $ Just a


paramOpt :: (Fn m, Fn.FromParam p) => Text -> Req -> m (Maybe (Req, (Either Fn.ParamError p -> a) -> a))
paramOpt n req =
  liftIO $ Fn.paramOpt n req


file :: Fn m => Text -> Req -> m (Maybe (Req, (Fn.File -> a) -> a))
file n req =
  liftIO $ Fn.file n req


files :: Fn m => Req -> m (Maybe (Req, ([(Text, Fn.File)] -> a) -> a))
files req =
  liftIO $ Fn.files req


--------------------------------------------------------------------------------
-- Responses


staticServe :: Fn m => Text -> m (Maybe Response)
staticServe d =
  getRequest >>= liftIO . Fn.staticServe d


sendFile :: FilePath -> IO (Maybe Response)
sendFile =
  liftIO . Fn.sendFile


okText :: Fn m => Text -> m (Maybe Response)
okText =
  liftIO . Fn.okText


okJson :: Fn m => Text -> m (Maybe Response)
okJson =
  liftIO . Fn.okJson


okHtml :: Fn m => Text -> m (Maybe Response)
okHtml =
  liftIO . Fn.okHtml


errText :: Fn m => Text -> m (Maybe Response)
errText =
  liftIO . Fn.errText


errHtml :: Fn m => Text -> m (Maybe Response)
errHtml =
  liftIO . Fn.errHtml


notFoundText :: Fn m => Text -> m Response
notFoundText =
  liftIO . Fn.notFoundText


notFoundHtml :: Fn m => Text -> m Response
notFoundHtml =
  liftIO . Fn.notFoundHtml


redirect :: Fn m => Text -> m (Maybe Response)
redirect =
  liftIO . Fn.redirect


redirectReferer :: Fn m => m (Maybe Response)
redirectReferer =
  getRequest >>= liftIO . Fn.redirectReferer



--------------------------------------------------------------------------------


text :: Fn m => Status -> ByteString -> Text -> m (Maybe Response)
text status content body =
  return $ Just $
    responseBuilder status [(Http.hContentType, content)] (Blaze.fromText body)


text200 :: Fn m => ByteString -> Text -> m (Maybe Response)
text200 content body =
  text Http.status200 content body


text301 :: Fn m => ByteString -> Text -> m (Maybe Response)
text301 content body =
  text Http.status301 content body


text302 :: Fn m => ByteString -> Text -> m (Maybe Response)
text302 content body =
  text Http.status302 content body


text403 :: Fn m => ByteString -> Text -> m (Maybe Response)
text403 content body =
  text Http.status403 content body


text404 :: Fn m => ByteString -> Text -> m (Maybe Response)
text404 content body =
  text Http.status404 content body


text410 :: Fn m => ByteString -> Text -> m (Maybe Response)
text410 content body =
  text Http.status410 content body


text500 :: Fn m => ByteString -> Text -> m (Maybe Response)
text500 content body =
  text Http.status500 content body


text503 :: Fn m => ByteString -> Text -> m (Maybe Response)
text503 content body =
  text Http.status503 content body


--------------------------------------------------------------------------------


html :: Fn m => Status -> Text -> m (Maybe Response)
html status body =
  text status "text/html; charset=utf-8" body


html200 :: Fn m => Text -> m (Maybe Response)
html200 =
  html Http.status200


html301 :: Fn m => Text -> m (Maybe Response)
html301 =
  html Http.status301


html302 :: Fn m => Text -> m (Maybe Response)
html302 =
  html Http.status302


html403 :: Fn m => Text -> m (Maybe Response)
html403 =
  html Http.status403


html404 :: Fn m => Text -> m (Maybe Response)
html404 =
  html Http.status404


html410 :: Fn m => Text -> m (Maybe Response)
html410 =
  html Http.status410


html500 :: Fn m => Text -> m (Maybe Response)
html500 =
  html Http.status500


html503 :: Fn m => Text -> m (Maybe Response)
html503 =
  html Http.status503


--------------------------------------------------------------------------------


json :: (Fn m, ToJSON a) => Status -> a -> m (Maybe Response)
json status val =
  return
    $ Just
    $ responseBuilder status [(Http.hContentType, "application/json; charset=utf-8")]
    $ lazyByteString
    $ encode val


json200 :: (Fn m, ToJSON a) => a -> m (Maybe Response)
json200 =
  json Http.status200


json301 :: (Fn m, ToJSON a) => a -> m (Maybe Response)
json301 =
  json Http.status301


json302 :: (Fn m, ToJSON a) => a -> m (Maybe Response)
json302 =
  json Http.status302


json403 :: (Fn m, ToJSON a) => a -> m (Maybe Response)
json403 =
  json Http.status403


json404 :: (Fn m, ToJSON a) => a -> m (Maybe Response)
json404 =
  json Http.status404


json410 :: (Fn m, ToJSON a) => a -> m (Maybe Response)
json410 =
  json Http.status410


json500 :: (Fn m, ToJSON a) => a -> m (Maybe Response)
json500 =
  json Http.status500


json503 :: (Fn m, ToJSON a) => a -> m (Maybe Response)
json503 =
  json Http.status503



--------------------------------------------------------------------------------


stream :: Fn m => ResponseHeaders -> StreamingBody -> m (Maybe Response)
stream headers body =
  return . Just $ responseStream Http.status200 headers body


streamFile :: Fn m => ByteString -> StreamingBody -> m (Maybe Response)
streamFile filename body =
  stream
    [ ( "Content-Disposition"
      , "attachment; filename=\"" <> filename <> "\""
      )
    ]
    body
