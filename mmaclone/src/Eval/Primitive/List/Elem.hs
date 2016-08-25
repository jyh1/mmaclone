module Eval.Primitive.List.Elem(
  -- * Elementary list manipulation function
  carl,cdrl,lengthl,consl) where
import Data.DataType
import Data.Number.Number
import Eval.Primitive.PrimiFunc
import Data.Environment.EnvironmentType

import Control.Monad.Except
import Data.List

lengthl, carl, cdrl, consl :: Primi
lengthl = do
  withnop 1
  [obj] <- getArgumentList
  return $ case obj of
            List x -> Number (genericLength x - 1)
            _ -> Number 0

carl = do
  withnop 1
  [obj] <- getArgumentList
  car obj

cdrl = do
  withnop 1
  [obj] <- getArgumentList
  cdr obj

consl = do
  withnop 2
  [a1, a2] <- getArgumentList
  cons a1 a2

car, cdr :: LispVal -> Primi
car (List []) = stateThrow (Default "car::empty list")
car (List (x:_)) = return x
car _ = noChange

cdr (List []) = stateThrow (Default "cdr:: empty list")
cdr (List (_:xs)) = return (List xs)
cdr _ = noChange

cons :: LispVal -> LispVal -> Primi
cons val (List xs) = return $ List (val : xs)
cons _ _ = stateThrow (Default "cons :: list expected")
