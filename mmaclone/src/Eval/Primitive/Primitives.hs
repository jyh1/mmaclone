module Eval.Primitive.Primitives(primitives') where

import Eval.Primitive.Primi.Primi
import Eval.Primitive.IOPrimi.IOPrimi
import Eval.Primitive.PrimiType


primitives' :: [(String, IOPrimi)]
primitives' = map (fmap toIOPrimi) primi ++ ioprimi
