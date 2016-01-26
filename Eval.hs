{-#LANGUAGE ExistentialQuantification#-}
module Eval
    (
    eval,
    evalWithRecord
    ) where

import DataType
import Number
import Primitives
import Environment

import Control.Monad
import Data.Ratio
import Data.Maybe(fromMaybe)

evalWithRecord :: Env -> Int -> LispVal -> IOThrowsError LispVal
evalWithRecord env nn val = do
  let n = integer $ fromIntegral nn
  evaled <- eval env val
  eval env (List [Atom "setDelayed", List [Atom "In", n], val])
  eval env (List [Atom "set", List [Atom "Out", n], evaled])

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val = do
  x1 <- eval' env val
  if x1 == val then return x1 else eval env x1

eval' :: Env -> LispVal -> IOThrowsError LispVal
eval' env (List [Atom "setDelayed", lhs, rhs]) =
  setVar env lhs rhs

eval' env (List [Atom "set", lhs, rhs]) = do
  evaled <- eval env rhs
  setVar env lhs evaled
  return evaled


eval' env (List (v:vs)) = do
  headE <- eval env v
  args <- mapM (eval env) vs
  let old = List (headE : args)
      getFName (Atom f) = Just f
      getFName _ = Nothing
  let fun = do
        name <- getFName headE
        lookup name primitives
  case fun of
    Just f -> liftThrows $ liftM (fromMaybe old) (f args)
    Nothing -> evalWithEnv env old

eval' env val@(Atom _) = evalWithEnv env val

eval' _ n@(Number (Rational r))
  | denominator r == 1 = return (Number $ Integer $ numerator r)
  | otherwise = return n

eval' _ x = return x

-- attribute relating functions

-- ----------------------------


-- ----------------------------


-- quote
-- quoted :: [LispVal] -> ThrowsError LispVal
-- quoted x = return $ List (Atom "quote" : x)

---------------------------------------------------
