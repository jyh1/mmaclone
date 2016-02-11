module Eval.Primitive.Primi.List.Elem(carl,cdrl,lengthl,consl) where
import Data.DataType 
import Data.Number.Number
import Eval.Primitive.PrimiType

import Control.Monad.Except
import Data.List

lengthl = sinop len
carl = sinop car
cdrl = sinop cdr
consl = binop cons

len :: SingleFun
len x = return $ Just $ len' x
        where
          len' (List x) = integer (genericLength x - 1)
          len' _ = integer 0

car ,cdr :: SingleFun
car (List []) = throwError (Default "car::empty list")
car (List (x:_)) = hasValue x
car _ = noChange

cdr (List []) = throwError (Default "cdr:: empty list")
cdr (List (_:xs)) = hasValue (List xs)
cdr _ = noChange

cons :: BinaryFun
cons val (List xs) = hasValue $ List (val : xs)
cons _ _ = throwError (Default "cons :: list expected")
