{-#LANGUAGE FlexibleContexts #-}

module Eval.Primitive.PrimiType where

import Data.DataType
import Data.Number.Number
import Data.Environment.Environment

import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.Except

-- * Types and common functions for defining primitive functions.

type Result = ThrowsError (Maybe LispVal)
type IOResult = IOThrowsError (Maybe LispVal)

-- Basic primitive function which only perform simple term rewriting
type Primi = [LispVal] -> Result
-- | Primitive function which will likely modifying enviroment or doing IO
type IOPrimi = Env -> [LispVal] -> IOResult
-- | Primitive function which would evaluate LispVal internally
type EvalPrimi = Eval -> [LispVal] -> IOResult

type SingleFun = LispVal -> Result
type BinaryFun = LispVal -> LispVal -> Result
type IOBinary = Env -> LispVal -> LispVal -> IOResult

type Primitives = M.Map String IOPrimi

type EvalResult = IOThrowsError LispVal

type Eval = LispVal -> EvalResult

-- | Pack a LispVal in IOResult or Result
hasValue :: (Monad m) => LispVal -> m (Maybe LispVal)
hasValue = return . Just

-- | Indicating that the evaluation will not provide new result
noChange :: (Monad m) => m (Maybe LispVal)
noChange = return Nothing

-- | Used in Eval.Primitvie.Primitives to transform Primi to IOPrimi
toIOPrimi :: Primi -> IOPrimi
toIOPrimi f _ ls = liftThrows $ f ls

-- | Obsoleted.
-- Unpack two values from a two-element-list and feed them to a function.
-- Throw an error otherwise.
binop name _ singleVal@[_] = throwError $ NumArgs name 2 singleVal
binop _ op [a, b] = op a b
binop name _ vals = throwError $ NumArgs name 2 vals

-- | Obsoleted.
-- Unpack exactly one value from a one-element-list.
-- Throw an error otherwise.
sinop _ op [x] = op x
sinop name _ vals  = throwError $ NumArgs name 1 vals

-- | Used to define primitive function which takes a
-- non-empty argument list.
many1op name _ [] = throwError $ NumArgs1 name
many1op _ f val = f val

-- | Used to define primitive function whose argument list's
-- length is between l and r.
manynop name l r f ls =
  let len = length ls in
    if l <= len && len <= r then f ls
      else throwError $ NumArgsN name l r len

-- | Ensure that the argument list has excatly n elements.
withnop n name f ls =
  let len = length ls in
    if n == len then f ls
      else throwError $ NumArgs name n ls
