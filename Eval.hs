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
import Attribute

import Control.Monad
import Data.Ratio
import Data.Maybe(fromMaybe)
import Data.List(sort)
evalWithRecord :: Env -> Int -> LispVal -> IOThrowsError LispVal
evalWithRecord env nn val = do
  let n = integer $ fromIntegral nn
  evaled <- eval env val
  eval env (List [Atom "SetDelayed", List [Atom "In", n], val])
  eval env (List [Atom "Set", List [Atom "Out", n], evaled])

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val = do
  x1 <- eval' env val
  if x1 == val then return x1 else eval env x1

eval' :: Env -> LispVal -> IOThrowsError LispVal
eval' env (List [Atom "SetDelayed", lhs, rhs]) =
  setVar env lhs rhs

eval' env (List [Atom "Set", lhs, rhs]) = do
  evaled <- eval env rhs
  setVar env lhs evaled
  return evaled


eval' env (List (v:vs)) = do
  let evalE = eval env
  headE <- evalE v
  args <- attributeEvaluate evalE headE vs
  -- args <- mapM (eval env) vs'
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
attributeEvaluate :: (LispVal -> IOThrowsError LispVal) ->
  LispVal -> [LispVal] -> IOThrowsError [LispVal]
attributeEvaluate evalE h rests = do
  let att = getAttributes h attributes
  evaled <- attEvalHold evalE att rests
  return $ allAttr att h evaled

attEvalHold:: (LispVal -> IOThrowsError LispVal) ->
  [Attribute] -> [LispVal] -> IOThrowsError [LispVal]
attEvalHold evalE atts vals
  | elem HoldAll atts = return vals
  | elem HoldFirst atts = do
      rest <- mapM evalE (tail vals)
      return (head vals : rest)
  | elem HoldRest atts = do
      first <- evalE (head vals)
      return (first : tail vals)
  | otherwise = mapM evalE vals

attEvalOrderless :: [Attribute] -> [LispVal] -> [LispVal]
attEvalOrderless att vals
  | Orderless `elem` att = sort vals
  | otherwise = vals

attEvalFlatten :: [Attribute] -> LispVal -> [LispVal] -> [LispVal]
attEvalFlatten att h vals
  | Flatten `elem` att = deleteSameHead vals h
  | otherwise = vals

attEvalSeqHold :: [Attribute] -> [LispVal] -> [LispVal]
attEvalSeqHold att vals
  | SequenceHold `elem` att = vals
  | otherwise = deleteSameHead vals (Atom "Sequence")

allAttr :: [Attribute] -> LispVal-> [LispVal] -> [LispVal]
allAttr att h = attEvalOrderless att .attEvalFlatten att h .
                  attEvalSeqHold att
-- ----------------------------

deleteSameHead :: [LispVal] -> LispVal -> [LispVal]
deleteSameHead [] _ = []
deleteSameHead (val@(List x):xs) h
  | head x == h = tail x ++ deleteSameHead xs h
  | otherwise = val : deleteSameHead xs h
deleteSameHead (x:xs) h = x : deleteSameHead xs h
