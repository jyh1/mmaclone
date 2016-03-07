module Eval.Primitive.EvalPrimi.Replace.ReplaceRepeated(replaceRepeatedl) where

import Eval.Primitive.PrimiType
import Eval.Primitive.Primi.Replace.Replace

replaceRepeatedl :: EvalPrimi
replaceRepeatedl eval =
  withnop 2 "ReplaceRepeated" (replaceRepeatedl' eval)
