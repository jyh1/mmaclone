module Eval.Primitive.IOPrimi.InOut.InOut
        (inl,outl) where

import Eval.Primitive.PrimiType
import Data.DataType
import Eval.Environment

import Data.IORef
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Except

inl, outl :: IOPrimi
inl = indexl "In"
outl = indexl "Out"

unpackError name = Default ("Machine-sized integer is expected in " ++ name)

-- unpack the index arguement in In or Out
unpack :: String -> LispVal -> IOThrowsError Integer
unpack name =
  unpackInt (unpackError name)

plusLine :: LispVal -> LispVal
plusLine val = List [Atom "Plus", atomLine, val]

index :: String -> IOPrimi
index name env [n] = do
  let fun = Atom name
  n' <- unpack name n
  if n' >= 0 then
    liftM Just $ evalWithEnv env (List [fun, n])
  else
    hasValue $ List [fun, plusLine n]

indexl name env = withnop 1 name (index name env)
