{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}


module Web.Fn.Digestive ( runForm ) where

--------------------------------------------------------------------------------
import           Control.Monad.Trans          ( liftIO )
import           Control.Monad.Trans.Resource ( runResourceT )
import           Data.Text                    ( Text )
import           Network.HTTP.Types.Method    ( methodPost )
import           Network.Wai                  ( Request (..) )
import           Text.Digestive               ( Form, View, postForm, getForm )
import           Web.Fn                       ( RequestContext(..) )
--------------------------------------------------------------------------------
import           Web.Fn.Digestive.Internal    ( requestFormEnv )
--------------------------------------------------------------------------------


-- | This function runs a form and passes the function in it's last
-- argument the result, which is a 'View' and an optional result. If
-- the request is a get, or if the form failed to validate, the result
-- will be 'Nothing' and you should render the form (with the errors
-- from the 'View').

runForm ::
  RequestContext ctxt =>
  ctxt -> Text -> Form v IO a -> ((View v, Maybe a) -> IO a1) -> IO a1
runForm ctxt nm frm k =
  runResourceT $
    let
      r = fst (getRequest ctxt)
    in
    if requestMethod r == methodPost then do
      env <- requestFormEnv (getRequest ctxt)
      r' <- liftIO $ postForm nm frm (const (return env))
      liftIO $ k r'
    else do
      r' <- (,Nothing) <$> liftIO (getForm nm frm)
      liftIO $ k r'
