{-# OPTIONS_HADDOCK hide       #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}


module Web.Fn.Internal where


--------------------------------------------------------------------------------
import qualified Blaze.ByteString.Builder.Char.Utf8 as Blaze
import           Control.Concurrent.MVar             ( MVar
                                                     , modifyMVar_
                                                     , readMVar
                                                     )
import           Control.Monad.Trans.Resource        ( InternalState
                                                     , createInternalState
                                                     )
import           Data.Aeson                          ( ToJSON )
import qualified Data.Aeson                         as Json
import           Data.ByteString                     ( ByteString )
import           Data.ByteString.Builder             ( lazyByteString )
import           Data.Text                           ( Text )
import qualified Data.Text                          as Text
import qualified Data.Text.Encoding                 as Text
import           Network.HTTP.Types                  ( Status )
import qualified Network.HTTP.Types                 as Http
import           Network.Wai                         ( Request
                                                     , Response
                                                     , responseBuilder
                                                     , defaultRequest
                                                     )
import           Network.Wai.Parse                   ( Param
                                                     , FileInfo(..)
                                                     , parseRequestBody
                                                     )
import qualified Network.Wai.Parse                  as Parse
import           System.Directory                    ( getTemporaryDirectory )
--------------------------------------------------------------------------------



type PostMVar =
  Maybe (MVar (Maybe (([Param], [Parse.File FilePath]), InternalState)))


-- | A normal WAI 'Request' and the parsed post body (if present). We can
-- only parse the body once, so we need to have our request (which we
-- pass around) to be able to have the parsed body.

type FnRequest =
  (Request, PostMVar)


-- | A default request, which is a WAI defaultRequest and a place for
-- an MVar where post info will be placed (if you parse the post
-- body).
--
-- Warning: If you try to parse the post body (with '!=>') without
-- replacing the Nothing placeholder with an actual MVar, it will blow
-- up!

defaultFnRequest :: FnRequest
defaultFnRequest =
  (defaultRequest, Nothing)


-- | An uploaded file.

data File =
  File
    { fileName        :: Text
    , fileContentType :: Text
    , filePath        :: FilePath
    }


data Store b a = Store b (b -> a)


instance Functor (Store b) where
  fmap f (Store b h) = Store b (f . h)


-- | Specify the way that Fn can get the 'FnRequest' out of your context.
--
-- The easiest way to instantiate this is to use the lens, but if you
-- don't want to use lenses, define 'getRequest' and 'setRequest'.
--
-- Note that 'requestLens' is defined in terms of 'getRequest' and
-- 'setRequest' and vice-versa, so you need to define _one_ of these.

class RequestContext ctxt where
  requestLens :: Functor f => (FnRequest -> f FnRequest) -> ctxt -> f ctxt
  requestLens f c =
    setRequest c <$> f (getRequest c)

  getRequest :: ctxt -> FnRequest
  getRequest c =
    let (Store r _) = requestLens (`Store` id) c
    in r

  setRequest :: ctxt -> FnRequest -> ctxt
  setRequest c r =
    let (Store _ b) = requestLens (`Store` id) c
    in b r


instance RequestContext FnRequest where
  getRequest   = id
  setRequest _ = id


--------------------------------------------------------------------------------


waiRedirect :: Monad m => Status -> Text -> m Response
waiRedirect status target =
  return
    $ responseBuilder status [(Http.hLocation, Text.encodeUtf8 target)]
    $ Blaze.fromText ""


waiText :: Monad m => Status -> ByteString -> Text -> m Response
waiText status contentType body =
  return
    $ responseBuilder status [(Http.hContentType, contentType)]
    $ Blaze.fromText body


waiPlainText :: Monad m => Status -> Text -> m Response
waiPlainText status body =
  waiText status "text/plain; charset=utf-8" body


waiHtml :: Monad m => Status -> Text -> m Response
waiHtml status body =
  waiText status "text/html; charset=utf-8" body


waiJson :: (Monad m, ToJSON a) => Status -> a -> m Response
waiJson status val =
  return
    $ responseBuilder
        status [(Http.hContentType, "application/json; charset=utf-8")]
    $ lazyByteString
    $ Json.encode val


--------------------------------------------------------------------------------


-- | Internal helper - uses the name of the file as the pattern.

tempFileBackEnd' ::
  InternalState -> ignored1 -> FileInfo () -> IO ByteString -> IO FilePath
tempFileBackEnd' is x fileInfo@(FileInfo name _ _) =
  Parse.tempFileBackEndOpts
    getTemporaryDirectory (Text.unpack $ Text.decodeUtf8 name) is x fileInfo


readBody ::
  MVar (Maybe (([Param], [Parse.File FilePath]), InternalState)) ->
  Request ->
  IO ()
readBody mv req =
  modifyMVar_ mv $
    \r ->
      case r of
        Nothing -> do
          is <- createInternalState
          rb <- parseRequestBody (tempFileBackEnd' is) req
          return (Just (rb, is))

        Just _ ->
          return r


getMVarParams :: PostMVar -> IO [Param]
getMVarParams Nothing = return []
getMVarParams (Just mv) =
  readMVar mv >>=
    \case
       Nothing             -> return []
       Just ((params,_),_) -> return params


getMVarFiles :: PostMVar -> Request -> IO [(Text, File)]
getMVarFiles postMV req =
  case postMV of
    Nothing ->
      error $
        "Fn: tried to read a 'file' or 'files', \
        \but FnRequest wasn't initialized with MVar."

    Just mv -> do
      -- NOTE(dbp 2016-03-25): readBody ensures that the value will be Just.
      readBody mv req
      Just ((_,fs),_) <- readMVar mv
      return $
        fmap
          ( \(n, FileInfo nm ct c) ->
              ( Text.decodeUtf8 n
              , File (Text.decodeUtf8 nm) (Text.decodeUtf8 ct) c
              )
          )
          fs
