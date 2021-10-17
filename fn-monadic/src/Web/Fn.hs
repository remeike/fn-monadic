{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}


{-|

This package provides a simple framework for routing and responses.
It exposes two different interfaces: a more generic, monad-based
interface that can be found in Web.Fn.Monadic and the IO-based interface
found in this module.

The two primary goals of the IO interface are:

1. All web handler functions are just plain IO. There is no Fn
monad, or monad transformer. This has a lot of nice properties,
foremost among them is that it is easier to call handlers from other
contexts (like GHCi, when testing, in other threads, etc). As a
result, these functions take a single extra parameter that
has the context that they need (like database connection pools, the
request, etc).

2. Web handlers are functions with typed parameters. When routing, we
specify many parameters (most commonly, numeric ids, but can be many
things), so the handlers should be functions that take those as
parameters.
-}


module Web.Fn
  ( -- * Application setup
    FnRequest
  , FnResponse
  , Fn.defaultFnRequest
  , RequestContext(..)
  , toWAI
    -- * Routing
  , Req
  , Route
  , route
  , Fn.fallthrough
  , (==>)
  , (!=>)
  , (Fn.//)
  , (/?)
  , Fn.path
  , Fn.end
  , Fn.anything
  , Fn.segment
  , Fn.method
  , Fn.FromParam(..)
  , Fn.ParamError(..)
  , Fn.param
  , Fn.paramBool
  , Fn.paramDef
  , Fn.paramMany
  , Fn.paramOpt
  , Fn.File(..)
  , Fn.file
  , Fn.files
    -- * Responses
  , staticServe
  , Fn.sendFile
  , Fn.okText
  , Fn.okJson
  , Fn.okHtml
  , Fn.errText
  , Fn.errHtml
  , Fn.notFoundText
  , Fn.notFoundHtml
  , Fn.notFoundJson
  , Fn.redirect
  , redirectReferer
  , Fn.skip
    -- * Utility Functions for Requests
  , waiRequest
  , waiRequestHeaders
  , lookupRequestHeader
  , getParams
  , lookupParam
  , decodeJsonBody
  -- * Helpers
  , tempFileBackEnd'
  ) where

--------------------------------------------------------------------------------
import           Data.Aeson               ( FromJSON )
import           Control.Monad.Reader     ( ReaderT(..) )
import           Data.Text                ( Text )
import           Network.HTTP.Types       ( RequestHeaders )
import           Network.Wai              ( Application, Request(..), Response )
import           Network.Wai.Parse        ( Param )
import           Web.Fn.Monadic           ( FnRequest, FnResponse, Req )
import qualified Web.Fn.Monadic          as Fn
--------------------------------------------------------------------------------
import           Web.Fn.Internal          ( RequestContext(..)
                                          , readBody
                                          , tempFileBackEnd'
                                          )
--------------------------------------------------------------------------------


-- | The type of a route, constructed with 'pattern ==> handler'.

type Route ctxt = ctxt -> Req -> IO (Maybe (IO FnResponse))


-- | Convert an Fn application (provide a context, a context to response
-- function and we'll create a WAI application by updating the 'FnRequest'
-- value for each call).

toWAI :: RequestContext ctxt => ctxt -> (ctxt -> IO Response) -> Application
toWAI ctxt f req cont =
  Fn.toWai (\app -> runReaderT app ctxt) (ReaderT f) req cont


-- | The main construct for Fn, 'route' takes a context (which it will pass
-- to all handlers) and a list of potential matches (which, once they
-- match, may still end up deciding not to handle the request - hence
-- the double 'Maybe'). It can be nested.
--
-- @
--  app ctxt =
--    route ctxt
--      [ end ==> index
--      , path "foo" \/\/ path "bar" \/\/ segment \/\/ param "id" ==> h
--      ]
--
--    where
--      index :: Ctxt -> Fn m => m FnResponse
--      index _ =
--        okText "This is the index."
--
--      h :: Fn m => Ctxt -> Text -> Text -> m FnResponse
--      h _ s i =
--        okText ("got path \/foo\/" <> s <> ", with id=" <> i)
-- @

route :: RequestContext ctxt => ctxt -> [Route ctxt] -> IO FnResponse
route ctxt pths =
  runReaderT (Fn.route (fmap liftRoute pths)) ctxt


liftRoute :: RequestContext ctxt => Route ctxt -> Fn.Route (ReaderT ctxt IO)
liftRoute r =
  \req -> ReaderT $ \ctxt -> fmap (fmap (\s -> ReaderT (\_ -> s))) (r ctxt req)


-- | The non-body parsing connective between route patterns and the
-- handler that will be called if the pattern matches. The type is not
-- particularly illuminating, as it uses polymorphism to be able to
-- match route patterns with varying numbers (and types) of parts with
-- functions of the corresponding number of arguments and types.

(==>) ::
  RequestContext ctxt =>
  (Req -> IO (Maybe (Req, k -> a))) ->
  (ctxt -> k) ->
  ctxt ->
  Req ->
  IO (Maybe a)
(match ==> handle) ctxt req =
  match req >>=
    \case
      Nothing ->
        return Nothing

      Just ((_,pathInfo',_,_,_), k) ->
        let
          (request, mv) = getRequest ctxt
        in
        return $ Just $ k $
          handle (setRequest ctxt (request { pathInfo = pathInfo' }, mv))


-- | The connective between route patterns and the handler that parses
-- the body, which allows post params to be extracted with 'Fn.param' and
-- allows 'Fn.file' to work (otherwise, it will trigger a runtime error).

(!=>) ::
  RequestContext ctxt =>
  (Req -> IO (Maybe (Req, k -> a))) ->
  (ctxt -> k) ->
  ctxt ->
  Req ->
  IO (Maybe a)
(match !=> handle) ctxt req =
  let
    (request, Just mv) = getRequest ctxt
  in
  readBody mv request >> match req >>=
    \case
      Nothing ->
        return Nothing

      Just ((_,pathInfo',_,_,_), k) ->
        return $ Just $ k $
          handle (setRequest ctxt (request { pathInfo = pathInfo' }, Just mv))



{-# DEPRECATED (/?) "Use the identical 'Fn.//' instead." #-}
-- | A synonym for 'Fn.//'. To be removed

(/?) ::
  (r -> IO (Maybe (r, k -> k'))) ->
  (r -> IO (Maybe (r, k' -> a))) ->
  r ->
  IO (Maybe (r, k -> a))
(/?) = (Fn.//)


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

staticServe :: RequestContext ctxt => Text -> ctxt -> IO FnResponse
staticServe d ctxt =
  runReaderT (Fn.staticServe d) ctxt


-- | Redirects to the referrer, if present in headers, else to "/".

redirectReferer :: RequestContext ctxt => ctxt -> IO FnResponse
redirectReferer ctxt =
  runReaderT Fn.redirectReferer ctxt



--------------------------------------------------------------------------------
-- Utility Functions for Requests


-- | Returns the WAI 'Request' directly.

waiRequest :: RequestContext ctxt => ctxt -> IO Request
waiRequest ctxt =
  runReaderT Fn.waiRequest ctxt


-- | Returns the WAI 'RequestHeaders' directly.

waiRequestHeaders :: RequestContext ctxt => ctxt -> IO RequestHeaders
waiRequestHeaders ctxt =
  runReaderT Fn.waiRequestHeaders ctxt


-- | Returns the value for the given request header key, if it exists.

lookupRequestHeader :: RequestContext ctxt => ctxt -> Text -> IO (Maybe Text)
lookupRequestHeader ctxt name =
  runReaderT (Fn.lookupRequestHeader name) ctxt


-- | Returns all of the query params in an 'FnRequest'

getParams :: RequestContext ctxt => ctxt -> IO [Param]
getParams ctxt =
  runReaderT Fn.getParams ctxt


-- | Returns the value for the given query parameter, parsed into the expected
-- type.

lookupParam ::
  (RequestContext ctxt, Fn.FromParam p) => ctxt -> Text -> IO (Maybe p)
lookupParam ctxt name =
  runReaderT (Fn.lookupParam name) ctxt


-- | Decode the JSON request body into the expected value.

decodeJsonBody ::
  (RequestContext ctxt, FromJSON a) => ctxt -> IO (Either Text a)
decodeJsonBody ctxt =
  runReaderT Fn.decodeJsonBody ctxt
