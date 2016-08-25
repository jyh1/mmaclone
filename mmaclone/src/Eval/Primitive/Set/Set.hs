module Eval.Primitive.Set.Set
        (setl,setDelayedl) where

import Data.DataType
import Eval.Primitive.PrimiFunc
import Data.Environment.Environment
import Data.Environment.EnvironmentType
import Data.Environment.Update
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
