module Eval.Primitive.IOPrimi.Set.Set
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

set :: IOBinary
set env lhs rhs = liftM Just $ setVar env lhs rhs

setl :: IOPrimi
setl env = binop (set env)

setDelayedl :: IOPrimi
setDelayedl env ls = setl env ls >> return (Just atomNull)

setVar :: Env -> Pattern -> LispVal -> IOThrowsError LispVal
setVar envRef lhs rhs =
  if validSet lhs then liftIO $ do
    match <- readIORef envRef
    let newCont = updateContext lhs rhs match
    writeIORef envRef newCont
    return rhs
  else
    throwError $ SetError lhs


-- setVar' :: Context -> Pattern -> LispVal -> Context
-- setVar' cont lhs rhs
