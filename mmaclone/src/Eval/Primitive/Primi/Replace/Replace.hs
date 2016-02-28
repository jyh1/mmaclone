module Eval.Primitive.Primi.Replace.Replace(replacel,replaceAlll) where

import Data.DataType
import Eval.Patt.Pattern
import Eval.Primitive.Primi.Replace.Unpack
import Eval.Primitive.Primi.List.Level
import Eval.Primitive.PrimiType

import Control.Monad.Except

replacel = manynop "Replace" 2 3 replacel'
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
