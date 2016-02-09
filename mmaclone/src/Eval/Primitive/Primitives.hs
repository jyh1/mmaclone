module Eval.Primitive.Primitives(primitives) where

import Eval.Primitive.Primi.Primi

import Control.Monad
import Control.Monad.Except
import Data.List(partition, genericLength, genericIndex,group)

primitives = primi
