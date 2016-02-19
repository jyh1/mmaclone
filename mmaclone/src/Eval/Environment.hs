module Eval.Environment(evalWithEnv) where
import Data.DataType
import Data.Environment.Environment

import Data.IORef
import Control.Monad

evalWithEnv :: Env -> LispVal -> IOThrowsError LispVal
evalWithEnv env lhs
  | validSet lhs = liftM (replaceContext lhs) (readCont env)
  | otherwise = return lhs
