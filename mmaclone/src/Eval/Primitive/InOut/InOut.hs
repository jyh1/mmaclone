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

index :: LispVal -> Context -> [LispVal] -> IOThrowsError LispVal
index fun@(Atom name) context [n] = do
  n' <- unpack name n
  if n' >= 0 then
    return $ replaceContext (List [fun, n]) context
  else
    return $ List [fun, plusLine n]

indexl :: Primi
indexl = do
  withnop 1
  context <- getCon
  h:args  <- getArgs
  lift $ index h context args
