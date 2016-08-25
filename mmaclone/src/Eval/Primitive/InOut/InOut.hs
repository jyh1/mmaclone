module Eval.Primitive.InOut.InOut
        (inl,outl) where

import Eval.Primitive.PrimiFunc
import Data.DataType
import Data.Environment.Update
import Data.Environment.EnvironmentType
import Data.Environment.Environment


import Data.IORef
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Except
import qualified Data.Text as T


inl, outl :: Primi
inl = indexl
outl = indexl

unpackError name = Default ("Machine-sized integer is expected in " `T.append` name)

-- unpack the index arguement in In or Out
unpack :: T.Text -> LispVal -> IOThrowsError Integer
unpack name =
  unpackInt (unpackError name)

plusLine :: LispVal -> LispVal
plusLine val = List [Atom "Plus", atomLine, val]

index :: LispVal -> Context -> [LispVal] -> Primi
index fun@(Atom name) context [n] = do
  n' <- lift (unpack name n)
  if n' >= 0 then
    replaceContext (List [fun, n]) context
  else
    return $ List [fun, plusLine n]

indexl :: Primi
indexl = do
  withnop 1
  context <- getCon
  h:args  <- getArgs
  index h context args
