module Eval.Primitive.IOPrimi.IOPrimi(ioprimi) where

import Eval.Primitive.IOPrimi.Set.Set
import Eval.Primitive.PrimiType


ioprimi :: [(String,IOPrimi)]
ioprimi = [
            ("Set",setl),
            ("SetDelayed", setDelayedl)
          ]
