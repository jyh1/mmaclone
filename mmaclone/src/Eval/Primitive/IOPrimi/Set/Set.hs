module Eval.Primitive.IOPrimi.Set.Set
        (setl,setDelayedl) where

import Eval.Primitive.PrimiType
import Data.DataType

import Data.IORef
import Control.Monad
import Control.Monad.IO.Class


set :: IOBinary
set env lhs rhs = liftM Just $ setVar env lhs rhs

setl :: IOPrimi
setl env = binop (set env)

setDelayedl :: IOPrimi
setDelayedl env ls = setl env ls >> return (Just atomNull)

setVar :: Env -> Pattern -> LispVal -> IOThrowsError LispVal
setVar envRef lhs rhs = liftIO $ do
  match <- readIORef envRef
  let newCont = setVar' match lhs rhs
  writeIORef envRef newCont
  return rhs

setVar' :: Context -> Pattern -> LispVal -> Context
setVar' cont lhs rhs
  | isPattern lhs = Context values (insertPattern patterns lhs rhs)
  | otherwise = Context (insertRule values lhs rhs) patterns
    where patterns = pattern cont
          values = value cont
