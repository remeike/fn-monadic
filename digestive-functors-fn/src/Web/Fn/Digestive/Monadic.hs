{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}


module Web.Fn.Digestive.Monadic ( runForm ) where

--------------------------------------------------------------------------------
import           Control.Monad.Trans           ( liftIO )
import           Control.Monad.Trans.Resource  ( runResourceT )
import           Data.Text                     ( Text )
import           Network.HTTP.Types.Method     ( methodPost )
import           Network.Wai                   ( Request (..) )
import           Text.Digestive                ( Form, View, postForm, getForm )
import           Web.Fn.Monadic                ( Fn(..) )
--------------------------------------------------------------------------------
import           Web.Fn.Digestive.Internal     ( requestFormEnv )
--------------------------------------------------------------------------------



-- | This function runs a form and passes the function in it's last
-- argument the result, which is a 'View' and an optional result. If
-- the request is a get, or if the form failed to validate, the result
-- will be 'Nothing' and you should render the form (with the errors
-- from the 'View').

runForm :: Fn m => Text -> Form v m a -> ((View v, Maybe a) -> m a1) -> m a1
runForm nm frm k = do
  fnReq <- getRequest
  if requestMethod (fst fnReq) == methodPost
    then do
      env <- liftIO $ runResourceT (requestFormEnv fnReq)
      req <- postForm nm frm (const (return (fmap liftIO env)))
      k req
    else do
      req <- (,Nothing) <$> getForm nm frm
      k req
