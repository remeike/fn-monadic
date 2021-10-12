{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}


module Web.Fn.Monadic.Digestive ( runForm ) where

--------------------------------------------------------------------------------
import           Control.Applicative            ( (<$>) )
import           Control.Arrow                  ( second )
import           Control.Concurrent.MVar        ( readMVar )
import           Control.Monad.Trans            ( liftIO )
import           Control.Monad.Trans.Resource   ( ResourceT
                                                , getInternalState
                                                , runResourceT
                                                )
import           Data.ByteString                ( ByteString )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text.Encoding            as T
import           Network.HTTP.Types.Method      ( methodPost )
import           Network.Wai                    ( Request (..) )
import           Network.Wai.Parse              ( File, FileInfo (..)
                                                , fileContent, parseRequestBody
                                                )
import           Text.Digestive                 ( Form, View, FormInput(..)
                                                , Env, fromPath, postForm
                                                , getForm
                                                )
import           Web.Fn.Monadic                 ( Fn(..), FnRequest
                                                , tempFileBackEnd'
                                                )
--------------------------------------------------------------------------------


queryFormEnv :: [(ByteString, Maybe ByteString)] -> [File FilePath] -> Env IO
queryFormEnv qs fs = \pth ->
  let qs' = map (TextInput . T.decodeUtf8 . fromMaybe "" . snd) $ filter (forSubForm pth) qs
      fs' = map (FileInput . fileContent . snd) $ filter (forSubForm pth) $ filter fileNameNotEmpty fs
  in return $ qs' ++ fs'
  where fileNameNotEmpty (_formName, fileInfo) = Network.Wai.Parse.fileName fileInfo /= "\"\""
        forSubForm pth = (==) (fromPath pth) . T.decodeUtf8 . fst


requestFormEnv :: FnRequest -> ResourceT IO (Env IO)
requestFormEnv req = do
  st <- getInternalState
  v <- case snd req of
         Nothing -> return Nothing
         Just mv -> liftIO (readMVar mv)
  (query, files) <-
     case v of
       Nothing -> liftIO $ parseRequestBody (tempFileBackEnd' st)
                                            (fst req)
       Just (q,_) -> return q
  return $ queryFormEnv (map (second Just) query ++ queryString (fst req)) files


-- | This function runs a form and passes the function in it's last
-- argument the result, which is a 'View' and an optional result. If
-- the request is a get, or if the form failed to validate, the result
-- will be 'Nothing' and you should render the form (with the errors
-- from the 'View').

runForm :: Fn m => Text -> Form v m a -> ((View v, Maybe a) -> m a1) -> m a1
runForm nm frm k = do
  fnReq <- getRequest
  if requestMethod (fst fnReq) == methodPost then do
    env <- liftIO $ runResourceT (requestFormEnv fnReq)
    req <- postForm nm frm (const (return (fmap liftIO env)))
    k req
  else do
    req <- (,Nothing) <$> getForm nm frm
    k req
