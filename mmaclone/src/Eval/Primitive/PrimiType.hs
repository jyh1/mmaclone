{-#LANGUAGE FlexibleContexts #-}

module Eval.Primitive.PrimiType where

import Data.DataType
import Data.Number.Number

import Control.Monad
import Control.Monad.Except

type Result = ThrowsError (Maybe LispVal)
type IOResult = IOThrowsError (Maybe LispVal)

type Primi = [LispVal] -> Result
type IOPrimi = Env -> [LispVal] -> IOResult

type SingleFun = LispVal -> Result
type BinaryFun = LispVal -> LispVal -> Result

hasValue :: (Monad m) => LispVal -> m (Maybe LispVal)
hasValue = return . Just

noChange :: (Monad m) => m (Maybe LispVal)
noChange = return Nothing


binop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
binop op [a, b] = op a b
binop _ vals = throwError $ NumArgs 2 vals

sinop op [x] = op x
sinop _ vals  = throwError $ NumArgs 1 vals

many1op _ [] = throwError NumArgs1
many1op f val = f val

liftEval f a b = return $ Just (f a b)