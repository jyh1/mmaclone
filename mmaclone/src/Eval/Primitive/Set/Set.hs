module Eval.Primitive.Set.Set
        (setl,setDelayedl) where

import Eval.Primitive.PrimiType
import Data.DataType
import Data.Environment.Environment
import Eval.Patt.Pattern

import Data.IORef
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Monad.Except


setl :: Primi
setl = do
  env <- getEnv
  [lhs, rhs] <- getArgumentList
  lift $ setVar env lhs rhs

setDelayedl :: Primi
setDelayedl = do
  setl
  return atomNull

setVar :: Env -> Pattern -> LispVal -> IOThrowsError LispVal
setVar envRef lhs rhs =
  if validSet lhs then liftIO $ do
    match <- readIORef envRef
    let newCont = updateContext lhs rhs match
    writeIORef envRef newCont
    return rhs
  else
    throwError $ SetError lhs
