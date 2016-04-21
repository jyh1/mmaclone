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
