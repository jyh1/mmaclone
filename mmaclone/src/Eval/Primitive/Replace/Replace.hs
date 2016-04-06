module Eval.Primitive.Primi.Replace.Replace
  (-- ^ Replace Functions
  replacel,replaceAlll,replaceRepeatedl') where

import Data.DataType
import Eval.Patt.Pattern
import Eval.Primitive.Primi.Replace.Unpack
import Eval.Primitive.Primi.List.Level
import Eval.Primitive.PrimiType
import Data.Environment.Environment

import Control.Monad.Except

-- | Replace
replacel = manynop "Replace" 2 3 replacel'
-- | ReplaceAll
replaceAlll = withnop 2 "ReplaceAll" replaceAlll'

replacel' :: Primi
replacel' (expr:rules:level) = do
  unpackedRules <- unpackReplaceArg rules
  levelSpeci <- unpackNormalLevelSpeci 0 level
  hasValue (levelSpeci (`tryReplaceRuleList` unpackedRules) expr)

replaceAlll' :: Primi
replaceAlll' [expr,rules] = do
  unpackedRules <- unpackReplaceArg rules
  hasValue $ replaceAll unpackedRules expr

-- | Replace until yielding no new result
replaceRepeated :: Eval -> LispVal -> (LispVal -> LispVal) -> EvalResult
replaceRepeated eval old replace = do
  new <- eval (replace old)
  if new == old then
    return new
  else
    replaceRepeated eval new replace

-- | Used in Eval.Primitive.EvalPrimi.Replace.ReplaceRepeated
replaceRepeatedl' :: Eval -> [LispVal] -> IOResult
replaceRepeatedl' eval [expr,rules] = do
  unpackedRules <- liftThrows $ unpackReplaceArg rules
  liftM Just (replaceRepeated eval expr (replaceAll unpackedRules))
