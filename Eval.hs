module Eval
    (
    eval
    ) where

import DataType

import Control.Monad
import Control.Monad.Except

eval :: LispVal -> ThrowsError LispVal
eval val@(List [Atom "quote", _]) = return val
eval (List (Atom func : args)) = apply func =<< mapM eval args
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction
                          "unrecognized primitive function args" func)
                        ($ args) $
                        lookup func primitives

primitives :: [(String,[LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quoteient", numericBinop quot),
              ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal]
  -> ThrowsError LispVal

numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = liftM (Number . foldl1 op) (mapM unpackNum params)

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum
