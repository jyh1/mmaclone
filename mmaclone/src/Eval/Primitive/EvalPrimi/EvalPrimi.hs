module Eval.Primitive.EvalPrimi.EvalPrimi(evalPrimi) where

import Eval.Primitive.PrimiType

import Eval.Primitive.EvalPrimi.Logic.AndOr
import Eval.Primitive.EvalPrimi.Replace.ReplaceRepeated
import Eval.Primitive.EvalPrimi.Nest.Nest


-- | these functions will be provided the eval function thus transformed
-- to IOPrimi in the Eval.Eval
evalPrimi :: [(String, EvalPrimi)]
evalPrimi = [
              ("And", andl)
            , ("Or", orl)
            , ("ReplaceRepeated", replaceRepeatedl)
            , ("Nest", nestl)
            , ("NestList", nestListl)
            ]
