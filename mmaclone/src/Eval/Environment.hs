module Eval.Environment(evalWithEnv) where
import Data.DataType
import Data.Environment.Environment
import Eval.Primitive.PrimiType

import Data.IORef
import Control.Monad
import Control.Lens

evalWithEnv :: Eval
evalWithEnv lhs
  | validSet lhs = uses con (replaceContext lhs)
  | otherwise = return lhs
