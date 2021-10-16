{-# OPTIONS_HADDOCK hide       #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}


module Web.Fn.Digestive.Internal ( requestFormEnv ) where

--------------------------------------------------------------------------------
import           Control.Arrow                  ( second )
import           Control.Concurrent.MVar        ( readMVar )
import           Control.Monad.Trans            ( liftIO )
import           Control.Monad.Trans.Resource   ( ResourceT
                                                , getInternalState
                                                )
import           Data.ByteString                ( ByteString )
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Text.Encoding            as T
import           Network.Wai                    ( Request (..) )
import           Network.Wai.Parse              ( File, FileInfo (..)
                                                , fileContent, parseRequestBody
                                                )
import           Text.Digestive                 ( FormInput(..), Env, fromPath )
import           Web.Fn                         ( FnRequest, tempFileBackEnd' )
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
