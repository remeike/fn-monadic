{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-|

This package provides a simple framework for routing and responses. The
two primary goals are:

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
  -- , Fn.paramMany
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
  , Fn.redirect
  , redirectReferer
    -- * Helpers
  , Fn.tempFileBackEnd'
  ) where

--------------------------------------------------------------------------------
import           Control.Monad.Trans.Reader ( ReaderT(..), ask, local )
import           Data.Text                  ( Text )
import           Network.Wai                ( Application, Request(..), Response )
import           Web.Fn.Monadic             ( Fn, FnRequest, Req )
import qualified Web.Fn.Monadic            as Fn
--------------------------------------------------------------------------------


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


-- | The type of a route, constructed with 'pattern ==> handler'.
type Route ctxt = ctxt -> Req -> IO (Maybe (IO (Maybe Response)))


-- | Specify the way that Fn can get the 'FnRequest' out of your context.
--
-- The easiest way to instantiate this is to use the lens, but if you
-- don't want to use lenses, define 'getRequest' and 'setRequest'.
--
-- Note that 'requestLens' is defined in terms of 'getRequest' and
-- 'setRequest' and vice-versa, so you need to define _one_ of these.

instance RequestContext ctxt => Fn (ReaderT ctxt IO) where
  getRequest = fmap getRequest ask
  setRequest req = local (\ctxt -> setRequest ctxt req)


-- | Convert an Fn application (provide a context, a context to response
-- function and we'll create a WAI application by updating the 'FnRequest'
-- value for each call).

toWAI :: RequestContext ctxt => ctxt -> (ctxt -> IO Response) -> Application
toWAI ctxt f req cont =
  Fn.toWai (\app -> (runReaderT app) ctxt) (ReaderT f) req cont


-- | The main construct for Fn, 'route' takes a context (which it will pass
-- to all handlers) and a list of potential matches (which, once they
-- match, may still end up deciding not to handle the request - hence
-- the double 'Maybe'). It can be nested.
--
-- @
-- app ctxt =
--   route ctxt
--     [ end ==> index
--     , path "foo" \/\/ path "bar" \/\/ segment \/? param "id" ==> h
--     ]
--
--   where
--     index :: Ctxt -> Fn m => m (Maybe Response)
--     index _ =
--       okText "This is the index."
--
--     h :: Fn m => Ctxt -> Text -> Text -> m (Maybe Response)
--     h _ s i =
--       okText ("got path \/foo\/" <> s <> ", with id=" <> i)
-- @

route :: RequestContext ctxt => ctxt -> [Route ctxt] -> IO (Maybe Response)
route ctxt pths =
  runReaderT (Fn.route (fmap liftRoute pths)) ctxt


liftRoute ::
  RequestContext ctxt => Route ctxt -> Fn.Route (ReaderT ctxt IO)
liftRoute r =
  \req -> ReaderT (\ctxt -> fmap (fmap (\s -> ReaderT (\_ -> s))) (r ctxt req))


-- | The non-body parsing connective between route patterns and the
-- handler that will be called if the pattern matches. The type is not
-- particularly illuminating, as it uses polymorphism to be able to
-- match route patterns with varying numbers (and types) of parts with
-- functions of the corresponding number of arguments and types.

(==>) ::
  RequestContext ctxt =>
  (Req -> IO (Maybe (Req, k -> IO (Maybe a)))) ->
  (ctxt -> k) ->
  ctxt ->
  Req ->
  IO (Maybe (IO (Maybe a)))
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
-- the body, which allows post params to be extracted with 'param' and
-- allows 'file' to work (otherwise, it will trigger a runtime error).

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
  Fn.readBody mv request >> match req >>=
    \case
      Nothing ->
        return Nothing

      Just ((_,pathInfo',_,_,_), k) ->
        return $ Just $ k $
          handle (setRequest ctxt (request { pathInfo = pathInfo' }, Just mv))



{-# DEPRECATED (/?) "Use the identical '//' instead." #-}
-- | A synonym for '//'. To be removed

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

staticServe :: RequestContext ctxt => Text -> ctxt -> IO (Maybe Response)
staticServe d ctxt =
  runReaderT (Fn.staticServe d) ctxt


-- | Redirects to the referrer, if present in headers, else to "/".

redirectReferer :: RequestContext ctxt => ctxt -> IO (Maybe Response)
redirectReferer ctxt =
  runReaderT Fn.redirectReferer ctxt
