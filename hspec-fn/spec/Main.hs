{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}

module Main where


----------------------------------------------------------
-- Section 0: Imports.                                  --
----------------------------------------------------------
import           Control.Applicative       ((<$>), (<*>))
import           Control.Concurrent.MVar   (MVar, isEmptyMVar, newEmptyMVar,
                                            newMVar, putMVar, takeMVar,
                                            tryPutMVar, tryTakeMVar)
import           Control.Lens              hiding ((.=))
import           Control.Monad             (void, when)
import           Control.Monad.IO.Class    (liftIO)
import           Data.Aeson                (FromJSON, ToJSON, Value (..),
                                            decode, object, parseJSON, toJSON,
                                            (.=))
import qualified Data.Aeson                as Ae ((.:))
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as BS (concat)
import           Data.Map                  (Map)
import qualified Data.Map                  as M
import           Data.Maybe                (fromMaybe)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import           Network.HTTP.Types.Method (StdMethod (..))
import           Network.Wai               (Response, requestMethod)
import           System.Directory          (doesFileExist, removeFile)
import           Text.Digestive
import           Web.Fn

import           Test.Hspec
import           Test.Hspec.Fn

----------------------------------------------------------
-- Section 1: Example application used for testing.     --
----------------------------------------------------------
data Foo = Foo Int String String
data Ctxt = Ctxt { _req   :: FnRequest
                 , _mv    :: MVar ()
                 , _store :: MVar (Map Int Foo) }

makeLenses ''Ctxt

instance RequestContext Ctxt where
  requestLens = req

newFoo :: Ctxt -> String -> String -> IO (Foo)
newFoo ctxt s1 s2 = do let smvar = view store ctxt
                       mp <- takeMVar smvar
                       let i = 1 + M.size mp
                       let foo = Foo i s1 s2
                       putMVar smvar (M.insert i foo mp)
                       return foo

lookupFoo :: Ctxt -> Int -> IO (Maybe Foo)
lookupFoo ctxt i = do let smvar = view store ctxt
                      mp <- takeMVar smvar
                      putMVar smvar mp
                      return (M.lookup i mp)

html :: Text
html = "<html><table><tr><td>One</td><td>Two</td></tr></table></html>"

testForm :: Form Text IO (Text, Text)
testForm = (,) <$> "a" .: check "Should not be empty" (not . T.null) (text Nothing)
               <*> "b" .: text Nothing

data ExampleObject = ExampleObject Integer Text deriving (Show, Eq)

instance ToJSON ExampleObject where
    toJSON (ExampleObject i t) = object [ "aNumber" .= i
                                        , "aString" .= t
                                        ]

instance FromJSON ExampleObject where
    parseJSON (Object o) = ExampleObject <$> o Ae..: "aNumber" <*>
                                             o Ae..: "aString"
    parseJSON _          = fail "Expected ExampleObject as JSON object"

exampleObj :: ExampleObject
exampleObj = ExampleObject 42 "foo"

paramsAndMethodHandler :: Ctxt -> Text -> IO (Maybe Response)
paramsAndMethodHandler ctxt q = do
  let m = requestMethod (fst $ _req ctxt)
  case m of
    "POST" -> okText $ methodAndParam "POST "
    _    -> okText "Not valid"
  where
    methodAndParam p = T.concat [p, q]

routes :: [Route Ctxt]
routes = [ method GET // path "test"    ==> const (okText html)
         , method POST // path "test"   ==> const (okText "")
         , method DELETE // path "test" ==> const (okText "deleted")
         , path "params" // param "q"   !=> paramsAndMethodHandler
         , path "redirect"              ==> const (redirect "/test")
         , path "setmv"                 ==> (\ctxt -> do let m = view mv ctxt
                                                         void $ liftIO $ tryPutMVar m ()
                                                         okText "")
         ]

site :: Ctxt -> IO Response
site ctxt = route ctxt routes `fallthrough` notFoundText "404"

initializer :: MVar (Map Int Foo) -> MVar () -> IO Ctxt
initializer state mvar = return (Ctxt defaultFnRequest mvar state)

----------------------------------------------------------
-- Section 2: Test suite against application.           --
----------------------------------------------------------

newtype FooFields = FooFields (IO String)

instance Factory Ctxt Foo FooFields where
  fields = FooFields (return "default")
  save (FooFields as) = do s <- liftIO as
                           eval (\ctxt -> newFoo ctxt s "const")

tests :: MVar (Map Int Foo) -> MVar () -> Spec
tests store' mvar =
  fn (initializer store' mvar) (\ctxt -> return $ toWAI ctxt site) []
                               (const $ return ()) $ do
    describe "requests" $ do
      it "should match selector from a GET request" $ do
        p <- get "/test"
        shouldHaveSelector "table td" p
        shouldNotHaveSelector "table td.doesntexist" p
        get "/redirect" >>= shouldNotHaveSelector "table td.doesntexist"
        get "/invalid_url" >>= shouldNotHaveSelector "table td.doesntexist"
      it "should have deleted as text on the response" $
        delete "/test" >>= shouldHaveText "deleted"
      it "should not match <html> on POST request" $
        post "/test" [] >>= shouldNotHaveText "<html>"
      it "should post parameters" $ do
        post "/params" [("q", "hello")] >>= shouldHaveText "POST hello"
        post "/params" [("r", "hello")] >>= shouldNotHaveText "hello"
      it "basic equality" $ do
        eval (\_ -> return 1) >>= shouldEqual (1::Integer)
        shouldNotEqual 1 (2::Integer)
      it "status code 200" $ do
        get "/test" >>= should200
        get "/invalid_url" >>= shouldNot200
      it "status code 404" $ do
        get "/test" >>= shouldNot404
        get "/invalid_url" >>= should404
      it "status code 3**" $ do
        get "/redirect" >>= should300
        get "/test" >>= shouldNot300
      it "status code 3** with target" $ do
        get "/redirect" >>= should300To "/test"
        get "/redirect" >>= shouldNot300To "/redirect"
        get "/test" >>= shouldNot300To "/redirect"
      it "differentiates between response content types" $ do
        Html _ doc <- get "/test"
        doc `shouldEqual` html
    describe "stateful changes" $ do
      let isE ctxt = isEmptyMVar (view mv ctxt)
      after (\_ -> void $ tryTakeMVar mvar) $
        it "should reflect stateful in handler" $ do
         eval isE >>= shouldEqual True
         void $ post "/setmv" []
         eval isE >>= shouldEqual False
         void $ post "/setmv" []
         eval isE >>= shouldEqual False
         eval (\ctxt -> void $ tryTakeMVar (view mv ctxt))
      it "cleans up" $ eval isE >>= shouldEqual True
      {-
    describe "forms" $ do
      it "should pass valid data" $ do
        form (Value ("foo", "bar")) testForm (M.fromList [("a", "foo"), ("b", "bar")])
        form (Value ("foo", "")) testForm (M.fromList [("a", "foo")])
      it "should fail on invalid data" $ do
        form (ErrorPaths ["a"]) testForm (M.fromList [("a", ""), ("b", "bar")])
        form (ErrorPaths ["a"]) testForm (M.fromList [("b", "bar")])
        form (ErrorPaths ["a"]) testForm (M.fromList [])
      it "should call predicates on valid data" $
        form (Predicate (("oo" `T.isInfixOf`) . fst)) testForm (M.fromList [("a", "foobar")])-}
              {-
    describe "sessions" $ do
      it "should be able to modify session in handlers" $
        recordSession $ do void $ get "/setsess/4"
                           sessionShouldContain "4"
                           sessionShouldContain "bar"
      it "should be able to modify session with eval" $
        recordSession $ do eval (with sess $ setInSession "foozlo" "bar" >> commitSession)
                           sessionShouldContain "foozlo"
                           sessionShouldContain "bar"
      it "should be able to persist sessions between requests" $
        recordSession $ do void $ get "/setsess/3"
                           get "/getsess/3" >>= shouldHaveText "bar"
      it "should be able to persist sessions between eval and requests" $
        recordSession $ do eval (with sess $ setInSession "2" "bar" >> commitSession)
                           get "/getsess/2" >>= shouldHaveText "bar"
      it "should be able to persist sessions between requests and eval" $
        recordSession $ do void $ get "/setsess/1"
                           eval (with sess $ getFromSession "1" ) >>= shouldEqual (Just "bar")
      it "should be able to persist sessions between eval and eval" $
        recordSession $ do eval (with sess $ setInSession "foofoo" "bar" >> commitSession)
                           eval (with sess $ getFromSession "foofoo" ) >>= shouldEqual (Just "bar")
      it "should be able to remove stuff from session" $
        recordSession $ do eval (with sess $ setInSession "foobar" "baz" >> commitSession)
                           sessionShouldContain "foobar"
                           eval (with sess $ deleteFromSession "foobar" >> commitSession)
                           sessionShouldNotContain "foobar"
                           -}
    describe "factories" $ do
      it "should be able to generate a foo" $
        do (Foo i _ _) <- create id
           Just (Foo _ _ s) <- eval (\ctxt -> lookupFoo ctxt i)
           s `shouldEqual` "const"
      it "should be able to modify defaulted values" $
        do (Foo _ s' _) <- create (\_ -> FooFields (return "Hi!"))
           s' `shouldEqual` "Hi!"
           (Foo _ s'' _) <- create id
           s'' `shouldNotEqual` "Hi!"


----------------------------------------------------------
-- Section 3: Code to interface with cabal test.        --
----------------------------------------------------------
main :: IO ()
main = do
  mvar <- newEmptyMVar
  store' <- newMVar M.empty
  hspec (tests store' mvar)
