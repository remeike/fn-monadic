{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}


{-|

This module provides a set of functions for responses. Because they
return generic 'Monad' instances, each function is compatible with
the interfaces exposed by both Web.Fn and Web.Fn.Monadic.
-}


module Web.Fn.Response
  ( FnResponse
    -- * Response Constructors
  , redirect3xx
  , text
  , bytestring
  , plain
  , html
  , json
  , stream
  , setResponseHeaders
    -- * Redirect Responses
  , redirect301
  , redirect302
  , redirect303
    -- * Plain Text Responses
  , plain200
  , plain201
  , plain202
  , plain400
  , plain401
  , plain403
  , plain404
  , plain410
  , plain500
    -- * HTML Responses
  , html200
  , html201
  , html202
  , html400
  , html401
  , html403
  , html404
  , html410
  , html500
    -- * JSON Responses
  , json200
  , json201
  , json202
  , json400
  , json401
  , json403
  , json404
  , json410
  , json500
    -- * File Responses
  , streamFile
  , bytestringFile
  ) where


--------------------------------------------------------------------------------
import           Data.Aeson              ( ToJSON )
import           Data.ByteString         ( ByteString )
import qualified Data.ByteString.Lazy   as LBS
import           Data.Text               ( Text )
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as TE
import           Network.HTTP.Types      ( Status, ResponseHeaders )
import qualified Network.HTTP.Types     as Http
import qualified Network.Mime           as Mime
import           Network.Wai             ( Response, StreamingBody )
import qualified Network.Wai            as Wai
import           System.FilePath         ( takeExtension )
--------------------------------------------------------------------------------
import           Web.Fn.Internal         ( waiRedirect, waiText, waiHtml
                                         , waiJson, waiPlainText
                                         )
--------------------------------------------------------------------------------


-- | A normal WAI 'Response' wrapped in 'Maybe. A route that return Nothing
-- will cause the Fn application to continue attempting to match on any
-- routes that might follow.

type FnResponse = Maybe Response


--------------------------------------------------------------------------------
-- Response Constructors


-- | Returns a redirect response with the given status code and url.
-- Note that the target is not validated, so it should be an absolute path/url.

redirect3xx :: Monad m => Status -> Text -> m FnResponse
redirect3xx status target =
  fmap Just $ waiRedirect status target


-- | Returns a response with the given status code, mime type, and 'Text' body.

text :: Monad m => Status -> ByteString -> Text -> m FnResponse
text status contentType body =
  fmap Just $ waiText status contentType body


-- | Returns a response with the given status code, mime type, and 'ByteString' body.

bytestring :: Monad m => Status -> ByteString -> LBS.ByteString -> m FnResponse
bytestring status contentType body =
  return . Just $ Wai.responseLBS status [(Http.hContentType, contentType)] body


-- | Returns a @text/plain@ response with the given 'Status' code
-- and a response body with the given 'Text'.

plain :: Monad m => Status -> Text -> m FnResponse
plain status body =
  fmap Just $ waiPlainText status body


-- | Returns a @text/html@ response with the given 'Status' code
-- and a response body with the given 'Text' as HTML.

html :: Monad m => Status -> Text -> m FnResponse
html status body =
  fmap Just $ waiHtml status body


-- | Returns a @application/json@ response with the given 'Status' code
-- and a response body with the JSON encoding of the given value.

json :: (Monad m, ToJSON a) => Status -> a -> m FnResponse
json status val =
  fmap Just $ waiJson status val


-- | Returns a streaming response with the given 'ResponseHeaders'
-- and 'StreamingBody'.

stream :: Monad m => ByteString -> StreamingBody -> m FnResponse
stream contentType body =
  return . Just $
    Wai.responseStream Http.status200 [(Http.hContentType, contentType)] body


-- | Convenience function for set the response headers for a given response.

setResponseHeaders ::
  Monad m =>
  (ResponseHeaders -> ResponseHeaders) -> m FnResponse -> m FnResponse
setResponseHeaders f mresp =
  mresp >>=
    \case
      Nothing   -> return Nothing
      Just resp -> return $ Just $ Wai.mapResponseHeaders f resp


--------------------------------------------------------------------------------
-- Redirect Responses


-- | Redirects to the given url with 301 status code (Moved Permanently).
-- Note that the target is not validated, so it should be an absolute path/url.

redirect301 :: Monad m => Text -> m FnResponse
redirect301 target =
  redirect3xx Http.status301 target


-- | Redirects to the given url with 302 status code (Found).
-- Note that the target is not validated, so it should be an absolute path/url.

redirect302 :: Monad m => Text -> m FnResponse
redirect302 target =
  redirect3xx Http.status302 target


-- | Redirects to the given url with 303 status code (See Other).
-- Note that the target is not validated, so it should be an absolute path/url.

redirect303 :: Monad m => Text -> m FnResponse
redirect303 target =
  redirect3xx Http.status303 target



--------------------------------------------------------------------------------
-- Plain Text Responses


-- | Returns a response with a 200 (OK) status code
-- and a response body with the given 'Text' as plain text.

plain200 :: Monad m => Text -> m FnResponse
plain200 =
  plain Http.status200


-- | Returns a response with a 201 (Created) status code
-- and a response body with the given 'Text' as plain text.

plain201 :: Monad m => Text -> m FnResponse
plain201 =
  plain Http.status201


-- | Returns a response with a 202 (Accepted) status code
-- and a response body with the given 'Text' as plain text.

plain202 :: Monad m => Text -> m FnResponse
plain202 =
  plain Http.status202


-- | Returns a response with a 400 (Bad Request) status code
-- and a response body with the given 'Text' as plain text.

plain400 :: Monad m => Text -> m FnResponse
plain400 =
  plain Http.status400


-- | Returns a response with a 401 (Unauthorized) status code
-- and a response body with the given 'Text' as plain text.

plain401 :: Monad m => Text -> m FnResponse
plain401 =
  plain Http.status401


-- | Returns a response with a 403 (Forbidden) status code
-- and a response body with the given 'Text' as plain text.

plain403 :: Monad m => Text -> m FnResponse
plain403 =
  plain Http.status403


-- | Returns a response with a 404 (Not Found) status code
-- and a response body with the given 'Text' as plain text.

plain404 :: Monad m => Text -> m FnResponse
plain404 =
  plain Http.status404


-- | Returns a response with a 410 (Gone) status code
-- and a response body with the given 'Text' as plain text.

plain410 :: Monad m => Text -> m FnResponse
plain410 =
  plain Http.status410


-- | Returns a response with a 500 (Internal Server Error) status code
-- and a response body with the given 'Text' as plain text.

plain500 :: Monad m => Text -> m FnResponse
plain500 =
  plain Http.status500



--------------------------------------------------------------------------------
-- HTML Responses


-- | Returns a response with a 200 (OK) status code
-- and a response body with the given 'Text' as HTML.

html200 :: Monad m => Text -> m FnResponse
html200 =
  html Http.status200


-- | Returns a response with a 201 (Created) status code
-- and a response body with the given 'Text' as HTML.

html201 :: Monad m => Text -> m FnResponse
html201 =
  html Http.status201


-- | Returns a response with a 202 (Accepted) status code
-- and a response body with the given 'Text' as HTML.

html202 :: Monad m => Text -> m FnResponse
html202 =
  html Http.status202


-- | Returns a response with a 400 (Bad Request) status code
-- and a response body with the given 'Text' as HTML.

html400 :: Monad m => Text -> m FnResponse
html400 =
  html Http.status400


-- | Returns a response with a 401 (Unauthorized) status code
-- and a response body with the given 'Text' as HTML.

html401 :: Monad m => Text -> m FnResponse
html401 =
  html Http.status401


-- | Returns a response with a 403 (Forbidden) status code
-- and a response body with the given 'Text' as HTML.

html403 :: Monad m => Text -> m FnResponse
html403 =
  html Http.status403


-- | Returns a response with a 404 (Not Found) status code
-- and a response body with the given 'Text' as HTML.

html404 :: Monad m => Text -> m FnResponse
html404 =
  html Http.status404


-- | Returns a response with a 410 (Gone) status code
-- and a response body with the given 'Text' as HTML.

html410 :: Monad m => Text -> m FnResponse
html410 =
  html Http.status410


-- | Returns a response with a 500 (Internal Server Error) status code
-- and a response body with the given 'Text' as HTML.

html500 :: Monad m => Text -> m FnResponse
html500 =
  html Http.status500



--------------------------------------------------------------------------------
-- JSON Responses


-- | Returns a response with a 200 (OK) status code
-- and a response body with the JSON encoding of the given value.

json200 :: (Monad m, ToJSON a) => a -> m FnResponse
json200 =
  json Http.status200


-- | Returns a response with a 201 (Created) status code
-- and a response body with the JSON encoding of the given value.

json201 :: (Monad m, ToJSON a) => a -> m FnResponse
json201 =
  json Http.status201


-- | Returns a response with a 202 (Accepted) status code
-- and a response body with the JSON encoding of the given value.

json202 :: (Monad m, ToJSON a) => a -> m FnResponse
json202 =
  json Http.status202


-- | Returns a response with a 400 (Bad Request) status code
-- and a response body with the JSON encoding of the given value.

json400 :: (Monad m, ToJSON a) => a -> m FnResponse
json400 =
  json Http.status400


-- | Returns a response with a 401 (Unauthorized) status code
-- and a response body with the JSON encoding of the given value.

json401 :: (Monad m, ToJSON a) => a -> m FnResponse
json401 =
  json Http.status401


-- | Returns a response with a 403 (Forbidden) status code
-- and a response body with the JSON encoding of the given value.

json403 :: (Monad m, ToJSON a) => a -> m FnResponse
json403 =
  json Http.status403


-- | Returns a response with a 404 (Not Found) status code
-- and a response body with the JSON encoding of the given value.

json404 :: (Monad m, ToJSON a) => a -> m FnResponse
json404 =
  json Http.status404


-- | Returns a response with a 410 (Gone) status code
-- and a response body with the JSON encoding of the given value.

json410 :: (Monad m, ToJSON a) => a -> m FnResponse
json410 =
  json Http.status410


-- | Returns a response with a 500 (Internal Service Error) status code
-- and a response body with the JSON encoding of the given value.

json500 :: (Monad m, ToJSON a) => a -> m FnResponse
json500 =
  json Http.status500



--------------------------------------------------------------------------------
-- File Responses


-- | Returns a streaming response with the given file name and 'StreamingBody'.

streamFile :: Monad m => Text -> StreamingBody -> m FnResponse
streamFile filename body =
  let
    contentType =
      Mime.defaultMimeLookup (Text.pack (takeExtension (Text.unpack filename)))

    contentDisposition =
      ( "Content-Disposition"
      , "attachment; filename=\"" <> TE.encodeUtf8 filename <> "\""
      )
  in
  setResponseHeaders (contentDisposition :) $ stream contentType body


-- | Returns a response with the given file name and lazy ByteString body.

bytestringFile :: Monad m => Text -> LBS.ByteString -> m FnResponse
bytestringFile filename body =
  let
    contentType =
      Mime.defaultMimeLookup (Text.pack (takeExtension (Text.unpack filename)))

    contentDisposition =
      ( "Content-Disposition"
      , "attachment; filename=\"" <> TE.encodeUtf8 filename <> "\""
      )
  in
  setResponseHeaders (contentDisposition :) $
    bytestring Http.status200 contentType body
