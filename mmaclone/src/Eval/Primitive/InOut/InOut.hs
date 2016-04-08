module Eval.Primitive.InOut.InOut
        (inl,outl) where

import Eval.Primitive.PrimiType
import Data.DataType
import Eval.Environment
import Data.Environment.Environment

import Data.IORef
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Except

inl, outl :: Primi
inl = indexl
outl = indexl

unpackError name = Default ("Machine-sized integer is expected in " ++ name)

-- unpack the index arguement in In or Out
unpack :: String -> LispVal -> IOThrowsError Integer
unpack name =
  unpackInt (unpackError name)

plusLine :: LispVal -> LispVal
plusLine val = List [Atom "Plus", atomLine, val]

index :: String -> Env -> [LispVal] -> IOThrowsError LispVal
index name env [n] = do
  let fun = Atom name
  n' <- unpack name n
  if n' >= 0 then
    evalWithEnv env (List [fun, n])
  else
    return $ List [fun, plusLine n]

indexl :: Primi
indexl = do
  withnop 1
  (Atom name)  <- getHead
  env <- getEnv
  args <- getArgumentList
  lift $ index name env args
