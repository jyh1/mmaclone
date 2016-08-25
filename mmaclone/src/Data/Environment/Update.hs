module Data.Environment.Update where

import Data.DataType
import Data.Environment.Environment
import Data.Environment.EnvironmentType
import Eval.Primitive.PrimiFunc

import Control.Lens hiding(List, Context)

setVariable :: LispVal -> LispVal -> StateResult ()
setVariable lhs rhs = updateCon (updateContext lhs rhs)

getVariable :: LispVal -> Primi
getVariable lhs = do
  context <- use con
  replaceContext lhs context
