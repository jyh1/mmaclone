{-#LANGUAGE FlexibleContexts #-}

module Eval.Primitive.PrimiType where

import Data.DataType
import Data.Number.Number
import Data.Environment.Environment

import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.Except

type Result = ThrowsError (Maybe LispVal)
type IOResult = IOThrowsError (Maybe LispVal)

type Primi = [LispVal] -> Result
type IOPrimi = Env -> [LispVal] -> IOResult

type SingleFun = LispVal -> Result
type BinaryFun = LispVal -> LispVal -> Result
type IOBinary = Env -> LispVal -> LispVal -> IOResult

type Primitives = M.Map String IOPrimi

hasValue :: (Monad m) => LispVal -> m (Maybe LispVal)
hasValue = return . Just

noChange :: (Monad m) => m (Maybe LispVal)
noChange = return Nothing

toIOPrimi :: Primi -> IOPrimi
toIOPrimi f _ ls = liftThrows $ f ls

binop name _ singleVal@[_] = throwError $ NumArgs name 2 singleVal
binop _ op [a, b] = op a b
binop name _ vals = throwError $ NumArgs name 2 vals

sinop _ op [x] = op x
sinop name _ vals  = throwError $ NumArgs name 1 vals

many1op name _ [] = throwError $ NumArgs1 name
many1op _ f val = f val

manynop name l r f ls =
  let len = length ls in
    if l <= len && len <= r then f ls
      else throwError $ NumArgsN name l r len

withnop n name f ls =
  let len = length ls in
    if n == len then f ls
      else throwError $ NumArgs name n ls


liftEval f a b = return $ Just (f a b)
