module Eval.Primitive.Replace.Replace
  (-- ^ Replace Functions
  replacel,replaceAlll,replaceRepeatedl) where

import Data.DataType
import Eval.Patt.Pattern
import Eval.Primitive.Replace.Unpack
import Eval.Primitive.List.Level
import Eval.Primitive.PrimiType
import Data.Environment.Environment

import Control.Monad.Except

replacel, replaceAlll :: Primi
replacel = do
  between 2 3
  usesArgumentError replacel'

replaceAlll = do
  withnop 2
  usesArgumentError replaceAlll'

replacel' :: EvalArguments
replacel' (expr:rules:level) = do
  unpackedRules <- unpackReplaceArg rules
  levelSpeci <- unpackNormalLevelSpeci 0 level
  return (levelSpeci (`tryReplaceRuleList` unpackedRules) expr)

replaceAlll' :: EvalArguments
replaceAlll' [expr,rules] = do
  unpackedRules <- unpackReplaceArg rules
  return $ replaceAll unpackedRules expr


-- functions relating with replace repeated feature
replaceRepeatedl :: Primi
replaceRepeatedl = do
  withnop 2
  getArgumentList >>= replaceRepeatedl'
-- | Replace until yielding no new result
replaceRepeated :: Eval -> LispVal -> (LispVal -> LispVal) -> IOThrowsError LispVal
replaceRepeated eval old replace = do
  new <- eval (replace old)
  if new == old then
    return new
  else
    replaceRepeated eval new replace

replaceRepeatedl' :: [LispVal] -> Primi
replaceRepeatedl' [expr,rules] = do
  eval <- getEval
  unpackedRules <- lift $ unpackReplaceArg rules
  lift $ replaceRepeated eval expr (replaceAll unpackedRules)
