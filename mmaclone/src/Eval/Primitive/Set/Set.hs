module Eval.Primitive.Set.Set
        (setl,setDelayedl,unsetl) where

import           Data.DataType
import           Data.Environment.Environment
import           Data.Environment.EnvironmentType
import           Data.Environment.Update
import           Eval.Patt.Pattern
import           Eval.Primitive.PrimiFunc

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.IORef


setl :: Primi
setl = do
  [lhs, rhs] <- getArgumentList
  setVar lhs rhs
  return rhs

setDelayedl :: Primi
setDelayedl = do
  setl
  return atomNull

setVar :: Pattern -> LispVal -> StateResult ()
setVar lhs rhs =
  if validSet lhs then setVariable lhs rhs
  else
    throwError $ SetError lhs

unsetl :: Primi
unsetl = do
   withnop 1
   [a] <- getArgumentList
   updateCon (unset a)
   return a
