{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Applicative     ((<$>))
import           Control.Concurrent.MVar
import           Data.Either
import           Data.Maybe
import           Data.Text               (Text)
import           Network.HTTP.Types
import           Network.Wai
import           System.IO.Unsafe
import           Test.Hspec
import           Web.Fn

emv = unsafePerformIO (newMVar Nothing)

instance Show (MVar a) where
  show _ = "<MVar>"

newtype R = R ([Text], Query)
instance RequestContext R where
  getRequest (R (p',q')) = (defaultRequest { pathInfo = p', queryString = q' }, Just emv)
  setRequest (R _) (r,_) = R (pathInfo r, queryString r)

rr :: R
rr = R ([], [])
p :: [Text] -> Req
p y = (defaultRequest,y,[],GET,Just emv)
_p :: [Text] -> Req ->  Req
_p y (r,_,q',m',x') = (r,y,q',m',x')
q :: Query -> Req
q y = (defaultRequest,[],y,GET,Just emv)
_q :: Query -> Req -> Req
_q y (r,p',_,m',x') = (r,p',y,m',x')
m :: StdMethod -> Req
m y = (defaultRequest,[],[],y,Just emv)
_m :: StdMethod -> Req -> Req
_m y (r,p',q',_,x') = (r,p',q',y,x')


j :: Show a => IO (Maybe (a,b)) -> Expectation
j mv = do x <- mv
          fst <$> x `shouldSatisfy` isJust
n :: Show a => IO (Maybe (a,b)) -> Expectation
n mv = do x <- mv
          fst <$> x `shouldSatisfy` isNothing
v :: IO (Maybe (a, t -> Bool)) -> t -> Expectation
v mv f = do x <- mv
            snd (fromJust x) f `shouldBe` True
vn :: IO (Maybe (a, t -> Bool)) -> t -> Expectation
vn mv f = do v <- mv
             case v of
               Nothing -> (1 :: Int) `shouldBe` 1
               Just (_,k) -> k f `shouldBe` False

shouldSatisfyIO a b = do x <- a
                         x `shouldSatisfy` b

t1 :: Text -> Text -> Bool
t1 = (==)
t2 :: Text -> Text -> Text -> Text -> Bool
t2 a b a' b' = a == a' && b == b'
t3 :: Text -> Text -> Text -> Text -> Text -> Text -> Bool
t3 a b c a' b' c' = a == a' && b == b' && c == c'

t1u :: Text -> Bool
t1u _ = undefined
t2u :: Text -> Text -> Bool
t2u _ _ = undefined
t3u :: Text -> Text -> Text -> Bool
t3u _ _ _ = undefined

main :: IO ()
main = hspec $ do

  describe "matching" $ do
    it "should match first segment with path" $
      do j (path "foo" (p ["foo", "bar"]))
         n (path "foo" (p []))
         n (path "foo" (p ["bar", "foo"]))
    it "should match two paths combined with //" $
      do j ((path "a" // path "b") (p ["a", "b"]))
         n ((path "b" // path "a") (p ["a", "b"]))
         n ((path "b" // path "a") (p ["b"]))
    it "should pass url segment to segment" $
      do v (segment (p ["a"])) (t1 "a")
         vn (segment (p [])) t1u
         v (segment (p ["a", "b"])) (t1 "a")
    it "should match two segments combined with //" $
      do v ((segment // segment) (p ["a", "b"])) (t2 "a" "b")
         vn ((segment // segment) (p [])) t2u
         v ((segment // segment) (p ["a", "b", "c"])) (t2 "a" "b")
    it "should match path and segment combined with //" $
      do v ((path "a" // segment) (p ["a", "b"])) (t1 "b")
         vn ((path "a" // segment) (p ["b", "b"])) t1u
         v ((segment // path "b") (p ["a", "b"])) (t1 "a")
    it "should match many segments and paths together" $
       do v ((path "a" // segment // path "c" // path "d")
             (p ["a","b","c", "d"])) (t1 "b")
          v ((segment // path "b" // segment // segment)
             (p ["a","b","c", "d", "e"])) (t3 "a" "c" "d")
          vn ((segment // path "b" // segment) (p ["a", "b"])) t2u
          vn ((segment // path "a" // segment) (p ["a", "b"])) t2u
    it "should match query parameters with param" $
      do v (param "foo" (q [("foo", Nothing)])) (t1 "")
         vn (param "foo" (q [])) t1u
    it "should map invalid bytestring param values to Unicode replacement character" $
         v (param "foo" (q [("foo", Just "bar\xc3")])) (t1 "bar\65533")
    it "should match combined param and paths with //" $
      do v ((path "a" // param "id") (_p ["a"] $ q [("id", Just "x")])) (t1 "x")
         vn ((path "a" // param "id") (_p ["b"] $ q [("id", Just "x")])) t1u
         vn ((path "a" // param "id") (_p [] $ q [("id", Just "x")])) t1u
         vn ((path "a" // param "id") (_p ["a"] $ q [("di", Just "x")])) t1u
    it "should match combining param, path, segment" $
      do v ((path "a" // segment // param "id")
             (_p ["a", "b"] $ q [("id", Just "x")])) (t2 "b" "x")
         vn ((path "a" // segment // segment // param "id")
               (_p ["a", "b"] $ q [("id", Just "x")])) t3u
    it "should apply matchers with ==>" $
      do (path "a" ==> const ()) rr (p ["a"])
           `shouldSatisfyIO` isJust
         (segment ==> \_ (_ :: Text) -> ()) rr (p ["1"])
            `shouldSatisfyIO` isJust
         (segment // path "b" ==> \_ x -> x == ("a" :: Text))
           rr (p ["a", "b"])
           `shouldSatisfyIO` fromJust
         (segment // path "b" ==> \_ x -> x == ("a" :: Text))
           rr (p ["a", "a"])
           `shouldSatisfyIO` isNothing
         (segment // path "b" ==> \_ x -> x == ("a" :: Text))
           rr (p ["a"])
           `shouldSatisfyIO` isNothing
    it "should always pass a value with paramOpt" $
      do x <- paramOpt "id" (q [])
         snd (fromJust x)
             (isLeft :: Either ParamError Text -> Bool)
             `shouldBe` True
         y <- paramOpt "id" (q [("id", Just "foo")])
         snd (fromJust y)
             (== Right ("foo" :: Text))
             `shouldBe` True
    it "should match end against no further path segments" $
      do j (end (p []))
         j (end (_p [] $ q [("foo", Nothing)]))
         n (end (p ["a"]))
    it "should match end after path and segments" $
      do j ((path "a" // end) (p ["a"]))
         v ((segment // end) (p ["a"])) (t1 "a")
    it "should match anything" $
      do j (anything (p []))
         j (anything (p ["f","b"]))

    it "should match against method" $
       do j (method GET (m GET))
          n (method GET (m POST))

  describe "route" $ do
    it "should match route to parameter" $
      do r <- route (R (["a"], [])) [segment ==> (\_ a -> if a == ("a"::Text) then okText "" else return Nothing)]
         (responseStatus <$> r) `shouldSatisfy` isJust
    it "should match nested routes" $
      do r <- route (R (["a", "b"], [])) [path "a" ==> (\c -> route c [path "b" ==> const (okText "")])]
         (responseStatus <$> r) `shouldSatisfy` isJust

  describe "okJson" $ do
    it "should have Content-Type: applcation/json as a header" $
      do maybeResponse <- okJson "{'key': 'value'}"
         let headers = responseHeaders $ fromJust maybeResponse
         headers `shouldBe` [(hContentType, "application/json; charset=utf-8")]

  describe "parameter parsing" $
    do it "should parse Text" $
         fromParam ["hello"] `shouldBe` Right ("hello" :: Text)
       it "should parse Int" $
         do fromParam ["1"] `shouldBe` Right (1 :: Int)
            fromParam ["2011"] `shouldBe` Right (2011 :: Int)
            fromParam ["aaa"] `shouldSatisfy`
              (isLeft :: Either ParamError Int -> Bool)
            fromParam ["10a"] `shouldSatisfy`
              (isLeft :: Either ParamError Int -> Bool)
       it "should be able to parse Double" $
         do fromParam ["1"] `shouldBe` Right (1 :: Double)
            fromParam ["1.02"] `shouldBe` Right (1.02 :: Double)
            fromParam ["thr"] `shouldSatisfy`
              (isLeft :: Either ParamError Double -> Bool)
            fromParam ["100o"] `shouldSatisfy`
              (isLeft :: Either ParamError Double -> Bool)
