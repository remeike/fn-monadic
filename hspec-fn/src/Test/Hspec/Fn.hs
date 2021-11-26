{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Test.Hspec.Fn (
  -- * Running blocks of hspec-snap tests
    fn
  , modifySite
  , modifySite'
  , afterEval
  , beforeEval

  -- * Core data types
  , TestResponse(..)
  , FnHspecM

  -- * Factory style test data generation
  , Factory(..)

  -- * Requests
  , delete
  , get
  , get'
  , post
  , post'
  , postJson
  , postJson'

  -- * Helpers for dealing with TestResponses
  , restrictResponse
  , withRequest
  , nowRun

  -- * Evaluating application code
  , eval

  -- * Unit test assertions
  , shouldChange
  , shouldEqual
  , shouldNotEqual
  , shouldBeTrue
  , shouldNotBeTrue

  -- * Response assertions
  , should200
  , shouldNot200
  , should404
  , shouldNot404
  , should300
  , shouldNot300
  , should300To
  , shouldNot300To
  , shouldHaveSelector
  , shouldNotHaveSelector
  , shouldHaveText
  , shouldNotHaveText

  -- * Form tests
  , FormExpectations(..)

  -- * Internal types and helpers
  , FnHspecState(..)
  , setResult
  , runRequest
  , runHandlerSafe
  , evalHandlerSafe
  ) where

import           Control.Applicative          ((<$>))
import           Control.Concurrent.MVar      (MVar, newEmptyMVar, newMVar,
                                               putMVar, readMVar, takeMVar)

import           Blaze.ByteString.Builder     (toByteString)
import           Control.Arrow                ((***))
import           Control.Exception            (SomeException, catch, throw)
import           Control.Monad                (void)
import           Control.Monad.State          (StateT (..), runStateT)
import qualified Control.Monad.State          as S (get, put)
import           Control.Monad.Trans          (liftIO)
import           Data.Aeson                   (ToJSON, encode)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as B (empty, unpack)
import           Data.ByteString.Lazy         (fromStrict, toStrict)
import qualified Data.ByteString.Lazy         as LBS
import           Data.ByteString.Lazy.Builder (Builder)
import qualified Data.ByteString.Lazy.Builder as Builder
import qualified Data.Char                    as Char
import           Data.IORef                   (atomicModifyIORef, newIORef,
                                               readIORef)
import           Data.List                    (intersperse)
import qualified Data.Map                     as M
import           Data.Maybe                   (fromJust, fromMaybe)
import           Data.Monoid                  (mappend, mconcat, mempty, (<>))
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import           Data.Word                    (Word8)
import           Network.HTTP.Types           (SimpleQuery, methodDelete,
                                               methodPost, simpleQueryToQuery)
import           Network.HTTP.Types.Header    (hContentType, hLocation)
import           Network.HTTP.Types.Status    (Status (..))
import           Network.Wai                  (Application, Middleware,
                                               Request (..), Response (..),
                                               defaultRequest, responseHeaders,
                                               responseStatus, responseToStream)
import           Network.Wai.Internal         (ResponseReceived (..))
import           Network.Wai.Test             (setPath)
import           Test.Hspec
import           Test.Hspec.Core.Spec         (Example (..), FailureReason (..),
                                               Location, Result (..), ResultStatus(..))
import qualified Text.Digestive               as DF
import qualified Text.HandsomeSoup            as HS
import qualified Text.XML.HXT.Core            as HXT
import           Web.Fn                       (RequestContext, defaultFnRequest,
                                               getRequest, okText, setRequest)

failShim :: Maybe Location -> String -> ResultStatus
failShim mLocation reason = Failure mLocation (Reason reason)
-- derives Num and Ord to avoid excessive newtype wrapping and unwrapping
-- in pattern matching, etc.

resultShim :: ResultStatus -> Result
resultShim resultStatus = Result "" resultStatus

newtype RespCode = RespCode Int deriving (Show, Read, Eq, Num, Ord)

-- | The result of making requests against your application. Most
-- assertions act against these types (for example, `should200`,
-- `shouldHaveSelector`, etc).
data TestResponse = Html RespCode Text
                  | Json RespCode LBS.ByteString
                  | NotFound
                  | Redirect RespCode Text
                  | Other RespCode
                  | Empty
                  deriving (Show, Eq)

-- | The main monad that tests run inside of. This allows both access
-- to the application (via requests and `eval`) and to running
-- assertions (like `should404` or `shouldHaveText`).
type FnHspecM b = StateT (FnHspecState b) IO

-- | Internal state used to share site initialization across tests, and to propogate failures.
-- Understanding it is completely unnecessary to use the library.
--
-- The fields it contains, in order, are:
--
-- > Result
-- > Core Application
-- > Application Middleware
-- > Startup state
-- > Session state
-- > Before handler (runs before each eval)
-- > After handler (runs after each eval).
data FnHspecState ctxt = FnHspecState ResultStatus
                                      Application
                                      [Middleware]
                                      ctxt
                                      (ctxt -> IO ())
                                      (ctxt -> IO ())

instance Example (FnHspecM b ()) where
  type Arg (FnHspecM b ()) = FnHspecState b
  evaluateExample s _ cb _ =
    do mv <- newEmptyMVar
       cb $ \st -> do
         r <- do ((),FnHspecState r' _ _ _ _ _) <- runStateT s st
                 return (Result "" r')
         putMVar mv r
       takeMVar mv

-- | Factory instances allow you to easily generate test data.
--
-- Essentially, you specify a default way of constructing a
-- data type, and allow certain parts of it to be modified (via
-- the 'fields' data structure).
--
-- An example follows:
--
-- > data Foo = Foo Int
-- > newtype FooFields = FooFields (IO Int)
-- > instance Factory App Foo FooFields where
-- >   fields = FooFields randomIO
-- >   save f = liftIO f >>= saveFoo . Foo1
-- >
-- > main = do create id :: FnHspecM App Foo
-- >           create (const $ FooFields (return 1)) :: FnHspecM App Foo
class Factory b a d | a -> b, a -> d, d -> a where
  fields :: d
  save :: d -> FnHspecM b a
  create :: (d -> d) -> FnHspecM b a
  create transform = save $ transform fields
  reload :: a -> FnHspecM b a
  reload = return

-- | The way to run a block of `FnHspecM` tests within an `hspec`
-- test suite. This takes both the top level handler (usually `route
-- routes`, where `routes` are all the routes for your site) and the
-- site initializer (often named `app`), and a block of tests. A test
-- suite can have multiple calls to `snap`, though each one will cause
-- the site initializer to run, which is often a slow operation (and
-- will slow down test suites).
fn :: IO ctxt -> (ctxt -> IO Application) -> [Middleware] -> (ctxt -> IO ()) -> SpecWith (FnHspecState ctxt) -> Spec
fn initializer mkApp middleware shutdown spec = do
  initCtxt <- runIO initializer
  application <- runIO (mkApp initCtxt)
  afterAll (const $ shutdown initCtxt) $
    before (return (FnHspecState Success application middleware initCtxt (const $ return ()) (const $ return ()))) spec

-- | This allows you to change the Application you are running
-- requests against within a block. This is most likely useful for
-- setting request state (for example, logging a user in).
modifySite :: (ctxt -> IO Middleware)
           -> SpecWith (FnHspecState ctxt)
           -> SpecWith (FnHspecState ctxt)
modifySite f =
  beforeWith (\(FnHspecState r app mids ctxt bef aft) ->
                do newmid <- liftIO $ f ctxt
                   return (FnHspecState r app (mids ++ [newmid]) ctxt bef aft))

-- | This performs a similar operation to `modifySite` but in the context
-- of `FnHspecM` (which is needed if you need to `eval`, produce values, and
-- hand them somewhere else (so they can't be created within `f`).
modifySite' :: (ctxt -> IO Middleware)
            -> FnHspecM ctxt a
            -> FnHspecM ctxt a
modifySite' f a = do (FnHspecState r app mids i bef aft) <- S.get
                     newmid <- liftIO $ f i
                     S.put (FnHspecState r app (mids ++ [newmid]) i bef aft)
                     a

-- | Evaluate a Handler action after each test.
afterEval :: (ctxt -> IO ()) -> SpecWith (FnHspecState ctxt) -> SpecWith (FnHspecState ctxt)
afterEval h = after (\(FnHspecState _r _site _ i _ _) ->
                       do res <- evalHandlerSafe h i
                          case res of
                            Right _ -> return ()
                            Left msg -> liftIO $ print msg)

-- | Evaluate a Handler action before each test.
beforeEval :: (ctxt -> IO ()) -> SpecWith (FnHspecState ctxt) -> SpecWith (FnHspecState ctxt)
beforeEval h = beforeWith (\state@(FnHspecState _r _site _ init _ _) -> do evalHandlerSafe h init
                                                                           return state)

-- | Runs a DELETE request
delete :: RequestContext ctxt => Text -> FnHspecM ctxt TestResponse
delete path = runRequest (setPath defaultRequest { requestMethod = methodDelete } (T.encodeUtf8 path))

-- | Runs a GET request.
-- | Runs a GET request, with a set of parameters.
get :: RequestContext ctxt =>  Text -> FnHspecM ctxt TestResponse
get path = runRequest (get' path)

get' :: Text -> Request
get' path = setPath defaultRequest (T.encodeUtf8 path)

-- | Creates a new POST request, with a set of parameters.
post :: RequestContext ctxt => Text -> SimpleQuery -> FnHspecM ctxt TestResponse
post path ps = do
   req <- liftIO $ post' path ps
   runRequest req

-- | Creates a new POST request, with a JSON body.
postJson :: (RequestContext ctxt, ToJSON tj) => Text -> tj -> FnHspecM ctxt TestResponse
postJson path blob = do
   req <- liftIO $ postJson' path blob
   runRequest req

postJson' :: ToJSON tj => Text -> tj -> IO Request
postJson' path blob = do
  let bod = encode blob
  refChunks <- newIORef $ LBS.toChunks bod
  let req = setPath defaultRequest { requestBody = atomicModifyIORef refChunks $ \bss ->
                                       case bss of
                                         [] -> ([], B.empty)
                                         x:y -> (y, x)
                                   , requestMethod = methodPost
                                   , requestHeaders = [(hContentType, "application/json")] }
                   (T.encodeUtf8 path)
  return req

post' :: Text -> SimpleQuery -> IO Request
post' path ps = do
  let bod = formUrlEncodeQuery (simpleQueryToParams ps)
  refChunks <- newIORef $ LBS.toChunks bod
  let req = setPath defaultRequest { requestBody = atomicModifyIORef refChunks $ \bss ->
                                       case bss of
                                         [] -> ([], B.empty)
                                         x:y -> (y, x)
                                   , requestMethod = methodPost
                                   , requestHeaders = [(hContentType, "application/x-www-form-urlencoded")] }
                   (T.encodeUtf8 path)
  return req

simpleQueryToParams :: SimpleQuery -> [(String, String)]
simpleQueryToParams = map (T.unpack . T.decodeUtf8 *** T.unpack . T.decodeUtf8)

{-
-- | Creates a new PUT request, with a set of parameters, with a default type of "application/x-www-form-urlencoded"
put :: Text -> SimpleQuery -> FnHspecM ctxt TestResponse
put path qs = put' path "application/x-www-form-urlencoded" (simpleQueryToParams qs)

-- | Creates a new PUT request with a configurable MIME/type
put' :: Text -> Text -> SimpleQuery -> FnHspecM ctxt TestResponse
put' path mime params' = runRequest $ do
  put'' (T.encodeUtf8 path) (T.encodeUtf8 mime) ""
  setQueryString (simpleQuerytoParams params')

put'' = undefined
setQueryString = undefined -}

formUrlEncodeQuery :: [(String, String)] -> LBS.ByteString
formUrlEncodeQuery = Builder.toLazyByteString . mconcat . intersperse amp . map encodePair
  where
    equals = Builder.word8 (ord '=')
    amp = Builder.word8 (ord '&')
    percent = Builder.word8 (ord '%')
    plus = Builder.word8 (ord '+')

    encodePair :: (String, String) -> Builder
    encodePair (key, value) = encode key <> equals <> encode value

    encode :: String -> Builder
    encode = escape . T.encodeUtf8 . T.pack . newlineNormalize

    newlineNormalize :: String -> String
    newlineNormalize input = case input of
      [] -> []
      '\n' : xs -> '\r' : '\n': newlineNormalize xs
      x : xs -> x : newlineNormalize xs

    escape :: ByteString -> Builder
    escape = mconcat . map f . B.unpack
      where
        f :: Word8 -> Builder
        f c
          | p c = Builder.word8 c
          | c == ord ' ' = plus
          | otherwise = percentEncode c

        p :: Word8 -> Bool
        p c =
             ord 'a' <= c && c <= ord 'z'
          || c == ord '_'
          || c == ord '*'
          || c == ord '-'
          || c == ord '.'
          || ord '0' <= c && c <= ord '9'
          || ord 'A' <= c && c <= ord 'Z'

    ord :: Char -> Word8
    ord = fromIntegral . Char.ord

    percentEncode :: Word8 -> Builder
    percentEncode n = percent <> hex hi <> hex lo
      where
        (hi, lo) = n `divMod` 16

    hex :: Word8 -> Builder
    hex n = Builder.word8 (offset + n)
      where
        offset
          | n < 10    = 48
          | otherwise = 55


-- | Restricts a response to matches for a given CSS selector.
-- Does nothing to non-Html responses.
restrictResponse :: Text -> TestResponse -> TestResponse
restrictResponse selector (Html code body) =
  case HXT.runLA (HXT.xshow $ HXT.hread HXT.>>> HS.css (T.unpack selector)) (T.unpack body) of
    [] -> Html code ""
    matches -> Html code (T.concat (map T.pack matches))
restrictResponse _ r = r

-- | Runs an arbitrary stateful action from your application.
eval :: (ctxt -> IO a) -> FnHspecM ctxt a
eval act = do (FnHspecState _ site _ ctxt bef aft) <- S.get
              liftIO $ either (error . T.unpack) id <$> evalHandlerSafe
                                                          (\ctxt -> do bef ctxt
                                                                       r <- act ctxt
                                                                       aft ctxt
                                                                       return r) ctxt

-- | Records a test Success or Fail. Only the first Fail will be
-- recorded (and will cause the whole block to Fail).
setResult :: ResultStatus -> FnHspecM ctxt ()
setResult r = do (FnHspecState r' s ms i bef aft) <- S.get
                 case r of
                   Success -> S.put (FnHspecState r s ms i bef aft)
                   _ -> throw r

-- | Asserts that a given stateful action will produce a specific different result after
-- an action has been run.
shouldChange :: (Show a, Eq a)
             => (a -> a)
             -> (ctxt -> IO a)
             -> FnHspecM ctxt c
             -> FnHspecM ctxt ()
shouldChange f v act = do before' <- eval v
                          void act
                          after' <- eval v
                          shouldEqual (f before') after'

-- | Asserts that two values are equal.
shouldEqual :: (Show a, Eq a)
            => a
            -> a
            -> FnHspecM ctxt ()
shouldEqual a b = if a == b
                      then setResult Success
                      else setResult (failShim Nothing ("Should have held: " ++ show a ++ " == " ++ show b))

-- | Asserts that two values are not equal.
shouldNotEqual :: (Show a, Eq a)
               => a
               -> a
               -> FnHspecM ctxt ()
shouldNotEqual a b = if a == b
                         then setResult (failShim Nothing ("Should not have held: " ++ show a ++ " == " ++ show b))
                         else setResult Success

-- | Asserts that the value is True.
shouldBeTrue :: Bool
             -> FnHspecM ctxt ()
shouldBeTrue True = setResult Success
shouldBeTrue False = setResult (failShim Nothing "Value should have been True.")

-- | Asserts that the value is not True (otherwise known as False).
shouldNotBeTrue :: Bool
                 -> FnHspecM ctxt ()
shouldNotBeTrue False = setResult Success
shouldNotBeTrue True = setResult (failShim Nothing "Value should have been True.")

-- | Asserts that the response is a success (either Html, or Other with status 200).
should200 :: TestResponse -> FnHspecM ctxt ()
should200 (Html _ _)   = setResult Success
should200 (Json 200 _) = setResult Success
should200 (Other 200)  = setResult Success
should200 r = setResult (failShim Nothing (show r))

-- | Asserts that the response is not a normal 200.
shouldNot200 :: TestResponse -> FnHspecM ctxt ()
shouldNot200 (Html _ _) = setResult (failShim Nothing "Got Html back.")
shouldNot200 (Other 200) = setResult (failShim Nothing "Got Other with 200 back.")
shouldNot200 _ = setResult Success

-- | Asserts that the response is a NotFound.
should404 :: TestResponse -> FnHspecM ctxt ()
should404 NotFound = setResult Success
should404 r = setResult (failShim Nothing (show r))

-- | Asserts that the response is not a NotFound.
shouldNot404 :: TestResponse -> FnHspecM ctxt ()
shouldNot404 NotFound = setResult (failShim Nothing "Got NotFound back.")
shouldNot404 _ = setResult Success

-- | Asserts that the response is a redirect.
should300 :: TestResponse -> FnHspecM ctxt ()
should300 (Redirect _ _) = setResult Success
should300 r = setResult (failShim Nothing (show r))

-- | Asserts that the response is not a redirect.
shouldNot300 :: TestResponse -> FnHspecM ctxt ()
shouldNot300 (Redirect _ _) = setResult (failShim Nothing "Got Redirect back.")
shouldNot300 _ = setResult Success

-- | Asserts that the response is a redirect, and thet the url it
-- redirects to starts with the given path.
should300To :: Text -> TestResponse -> FnHspecM ctxt ()
should300To pth (Redirect _ to) | pth `T.isPrefixOf` to = setResult Success
should300To _ r = setResult (failShim Nothing (show r))

-- | Asserts that the response is not a redirect to a given path. Note
-- that it can still be a redirect for this assertion to succeed, the
-- path it redirects to just can't start with the given path.
shouldNot300To :: Text -> TestResponse -> FnHspecM ctxt ()
shouldNot300To pth (Redirect _ to) | pth `T.isPrefixOf` to = setResult (failShim Nothing "Got Redirect back.")
shouldNot300To _ _ = setResult Success

-- | Assert that a response (which should be Html) has a given selector.
shouldHaveSelector :: Text -> TestResponse -> FnHspecM ctxt ()
shouldHaveSelector selector r@(Html _ body) =
  setResult $ if haveSelector' selector r
                then Success
                else failShim Nothing msg
  where msg = T.unpack $ T.concat ["Html should have contained selector: ", selector, "\n\n", body]
shouldHaveSelector match _ = setResult (failShim Nothing (T.unpack $ T.concat ["Non-HTML body should have contained css selector: ", match]))

-- | Assert that a response (which should be Html) doesn't have a given selector.
shouldNotHaveSelector :: Text -> TestResponse -> FnHspecM ctxt ()
shouldNotHaveSelector selector r@(Html _ body) =
  setResult $ if haveSelector' selector r
                then failShim Nothing msg
                else Success
  where msg = T.unpack $ T.concat ["Html should not have contained selector: ", selector, "\n\n", body]
shouldNotHaveSelector _ _ = setResult Success

haveSelector' :: Text -> TestResponse -> Bool
haveSelector' selector (Html _ body) =
  case HXT.runLA (HXT.hread HXT.>>> HS.css (T.unpack selector)) (T.unpack body)  of
    [] -> False
    _ -> True
haveSelector' _ _ = False

-- | Asserts that the response (which should be Html) contains the given text.
shouldHaveText :: Text -> TestResponse -> FnHspecM ctxt ()
shouldHaveText match (Html _ body) =
  if T.isInfixOf match body
  then setResult Success
  else setResult (failShim Nothing $ T.unpack $ T.concat [body, "' does not contain '", match, "'."])
shouldHaveText match resp = setResult (failShim Nothing (T.unpack $ T.concat [T.pack (show resp), " does not contain: ", match]))

-- | Asserts that the response (which should be Html) does not contain the given text.
shouldNotHaveText :: Text -> TestResponse -> FnHspecM ctxt ()
shouldNotHaveText match (Html _ body) =
  if T.isInfixOf match body
  then setResult (failShim Nothing $ T.unpack $ T.concat [body, "' contains '", match, "'."])
  else setResult Success
shouldNotHaveText _ _ = setResult Success

-- | Sets the request used by any calls to 'eval' within the block.
-- When combined with 'runRequest', you should be able to 'eval' on
-- the same request as a request. Note: we run the request _through_
-- the middleware first!
withRequest :: (RequestContext b) => Request -> FnHspecM b a -> FnHspecM b a
withRequest rq' t = do (FnHspecState r app mids ctxt bef aft) <- S.get
                       let (_, mv) = getRequest ctxt
                       reqmv <- liftIO $ newEmptyMVar
                       liftIO $ (foldr ($) (\req cont -> do putMVar reqmv req
                                                            cont =<< fromJust <$> okText "")
                                           mids) rq' (\resp -> return ResponseReceived)
                       rq <- liftIO $ takeMVar reqmv
                       S.put (FnHspecState r app mids (setRequest ctxt (rq, mv)) bef aft)
                       res <- t
                       S.put (FnHspecState r app mids ctxt bef aft)
                       return res



-- class HasSession b where
--   getSession :: b -> Vault.Key (Session IO Text (Maybe Text))

-- recordSession :: (RequestContext b, HasSession b) => FnHspecM b a -> FnHspecM b a
-- recordSession a =
--   do (FnHspecState r site s ctxt mv bef aft) <- S.get
--      S.put (FnHspecState r site s i mv
--             (do ps <- liftIO $ readMVar mv
--                 let Just (_, setsess) = Vault.lookup (getSession ctxt) (vault (fst (getRequest ctxt)))
--                 mapM_ (uncurry setsess) ps)
--             (do let Just (getsess) = Vault.lookup (getSession ctxt) (vault (fst (getRequest ctxt)))

--                 ps' <- with getSessionLens sessionToList
--                 void . liftIO $ takeMVar mv
--                 liftIO $ putMVar mv ps'))
--      res <- a
--      (SnapHspecState r' _ _ _ _ _ _) <- S.get
--      void . liftIO $ takeMVar mv
--      liftIO $ putMVar mv []
--      S.put (SnapHspecState r' site s i mv bef aft)
--      return res

-- sessContents :: SnapHspecM b Text
-- sessContents = do
--   (SnapHspecState _ _ _ _ mv _ _) <- S.get
--   ps <- liftIO $ readMVar mv
--   return $ T.concat (map (uncurry T.append) ps)

-- sessionShouldContain :: Text -> SnapHspecM b ()
-- sessionShouldContain t =
--   do contents <- sessContents
--      if t `T.isInfixOf` contents
--        then setResult Success
--        else setResult (failShim Nothing $ "Session did not contain: " ++ T.unpack t
--                                     ++ "\n\nSession was:\n" ++ T.unpack contents)

-- sessionShouldNotContain :: Text -> SnapHspecM b ()
-- sessionShouldNotContain t =
--   do contents <- sessContents
--      if t `T.isInfixOf` contents
--        then setResult (failShim Nothing $ "Session should not have contained: " ++ T.unpack t
--                                     ++ "\n\nSession was:\n" ++ T.unpack contents)
--        else setResult Success

-- | A data type for tests against forms.
data FormExpectations a = Value a           -- ^ The value the form should take (and should be valid)
                        | Predicate (a -> Bool)
                        | ErrorPaths [Text] -- ^ The error paths that should be populated

{-
-- | Tests against digestive-functors forms.
form :: (Eq a, Show a)
     => FormExpectations a           -- ^ If the form should succeed, Value a is what it should produce.
                                     --   If failing, ErrorPaths should be all the errors that are triggered.
     -> DF.Form Text (ctxt -> IO Response) a -- ^ The form to run
     -> M.Map Text Text                -- ^ The parameters to pass
     -> FnHspecM ctxt ()
form expected theForm theParams =
  do r <- eval $ DF.postForm "form" theForm (const $ return lookupParam)
     case expected of
       Value a -> shouldEqual (snd r) (Just a)
       Predicate f ->
         case snd r of
           Nothing -> setResult (failShim Nothing $ T.unpack $
                                 T.append "Expected form to validate. Resulted in errors: "
                                          (T.pack (show $ DF.viewErrors $ fst r)))
           Just v -> if f v
                       then setResult Success
                       else setResult (failShim Nothing $ T.unpack $
                                       T.append "Expected predicate to pass on value: "
                                                (T.pack (show v)))
       ErrorPaths expectedPaths ->
         do let viewErrorPaths = map (DF.fromPath . fst) $ DF.viewErrors $ fst r
            if all (`elem` viewErrorPaths) expectedPaths
               then if length viewErrorPaths == length expectedPaths
                       then setResult Success
                       else setResult (failShim Nothing $ "Number of errors did not match test. Got:\n\n "
                                            ++ show viewErrorPaths
                                            ++ "\n\nBut expected:\n\n"
                                            ++ show expectedPaths)
               else setResult (failShim Nothing $ "Did not have all errors specified. Got:\n\n"
                                    ++ show viewErrorPaths
                                    ++ "\n\nBut expected:\n\n"
                                    ++ show expectedPaths)
  where lookupParam pth = case M.lookup (DF.fromPath pth) fixedParams of
                            Nothing -> return []
                            Just v -> return [DF.TextInput v]
        fixedParams = M.mapKeys (T.append "form.") theParams
-}

-- | Runs a request (built with helpers), resulting in a response.
runRequest :: RequestContext ctxt => Request -> FnHspecM ctxt TestResponse
runRequest = runRequest' True

-- | Runs the request that is in the context. Note: you should only do
-- this after you've used 'withRequest'.
nowRun :: RequestContext ctxt => FnHspecM ctxt TestResponse
nowRun = do (FnHspecState _ _ _ ctxt _ _) <- S.get
            runRequest' False (fst . getRequest $ ctxt)

runRequest' :: RequestContext ctxt => Bool -> Request -> FnHspecM ctxt TestResponse
runRequest' runMiddleware req = do
  (FnHspecState _ application mids is bef aft) <- S.get
  res <- liftIO $ runHandlerSafe req
                  (\ctxt -> do bef ctxt
                               mv <- newEmptyMVar
                               let app = if runMiddleware
                                            then foldr ($) application mids
                                            else application
                               app req (\resp -> do putMVar mv resp
                                                    return ResponseReceived)
                               aft ctxt
                               takeMVar mv) is
  case res of
    Left err ->
      error $ T.unpack err
    Right response -> let respCode = RespCode $ statusCode $ responseStatus response in
      case respCode of
        404 -> return NotFound
        200 -> liftIO $ parse200 response
        _   -> if respCode >= 300 && respCode < 400
                then do let headers = responseHeaders response
                        let url = fromMaybe "" (lookup hLocation headers)
                        return (Redirect respCode (T.decodeUtf8 url))
                else return (Other respCode)

getResponseBody :: Response -> IO ByteString
getResponseBody res = do
    refBuilder <- newIORef mempty
    let add y = atomicModifyIORef refBuilder $ \x -> (x `mappend` y, ())
    withBody $ \body -> body add (return ())
    builder <- readIORef refBuilder
    return $ toByteString builder
  where
    (_, _, withBody) = responseToStream res

parse200 :: Response -> IO TestResponse
parse200 resp =
    let body        = getResponseBody resp
        headers     = responseHeaders resp
        contentType = T.decodeUtf8 $ fromMaybe "" $ lookup hContentType headers in
    if "application/json" `T.isPrefixOf` contentType
      then Json 200 . fromStrict <$> body
      else Html 200 . T.decodeUtf8 <$> body

-- | Runs a request against a given handler (often the whole site),
-- with the given state. Returns any triggered exception, or the response.
runHandlerSafe :: RequestContext ctxt
               =>  Request
               -> (ctxt -> IO Response)
               -> ctxt
               -> IO (Either Text (Response))
runHandlerSafe req site ctxt =
  do mv <- newMVar Nothing
     catch (Right <$> (site $ setRequest ctxt (req, Just mv)))
      (\(e::SomeException) ->
        return $ Left (T.pack $ show e))

-- | Evaluates a given handler with the given state. Returns any
-- triggered exception, or the value produced.
evalHandlerSafe :: (ctxt -> IO a)
                -> ctxt
                -> IO (Either Text a)
evalHandlerSafe act ctxt =
  catch (Right <$> act ctxt) (\(e::SomeException) -> return $ Left (T.pack $ show e))
