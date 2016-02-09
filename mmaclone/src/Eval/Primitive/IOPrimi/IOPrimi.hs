module Eval.Primitive.Primitives.IOPrimi.IOPrimi(ioprimi) where

import Data.DataType
import Data.Number.Number

import Control.Monad
import Control.Monad.Except
import Data.List(partition, genericLength, genericIndex,group)


ioprimi :: [(String,IOPrimi)]
ioprimi = [
            ("Set",set),
            ("SetDealyed", set)
          ]
