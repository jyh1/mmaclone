module Eval.Primitive.Arithmatic.Arithmatic
        (
        -- * Functions related with arithmatic. Plus, Times, Power etc...
        plusl,timesl,powerl,dividel,minusl,logl) where
import Data.DataType
import Data.Number.Number
import Eval.Primitive.PrimiFunc
import Data.Environment.EnvironmentType

import Control.Monad
import Control.Monad.Except
import Data.List


timesOrPlus :: (Number -> Number -> Number) ->
  Number ->
  (Number -> [LispVal] -> Primi) ->
  Primi
timesOrPlus mp zero merge = do
  (nums, syms) <- fmap (partition checkNum) getArgumentList
  let unpacked = map unpackNum nums
      ans = foldl mp zero unpacked
  merge ans syms

mergePlus,mergeTimes :: Number -> [LispVal] -> Primi
mergePlus num [] = return (Number num)
mergePlus 0 xs = tagHead xs
mergePlus num xs = tagHead (Number num : xs)

mergeTimes num [] = return (Number num)
mergeTimes 0 xs = return (Number 0)
mergeTimes 1 xs = tagHead xs
mergeTimes num xs = tagHead (Number num : xs)

-- groupPlus,groupTimes :: [LispVal] -> LispVal
-- groupPlus [single] = single
-- groupPlus xs = List [Atom "Times", Number (genericLength xs), head xs]
--
-- groupTimes [single] = single
-- groupTimes xs = List [Atom "Power", head xs, Number (genericLength xs)]
-- --------------------------------------------------
-- | expected exactly two arguments
minus, divide :: [LispVal] -> LispVal
minus [Number a, Number b] = Number $  a - b
minus [a, b] = minus' a b
  where
    minus' a b = List [Atom "Plus", a, List [Atom "Times", Number (-1), b]]

divide [Number a, Number b] = Number $ a / b
divide [a, b] = divide' a b
  where
    divide' a b = List [Atom "Times", a, List [Atom "Power", b, Number (-1)]]

minusOrDivide :: ([LispVal] -> LispVal) -> Primi
minusOrDivide f = do
  withnop 2
  fmap f getArgumentList

minusl, dividel, timesl, plusl :: Primi
minusl = minusOrDivide minus
dividel = minusOrDivide divide
timesl = timesOrPlus (*) 1 mergeTimes
plusl = timesOrPlus (+) 0 mergePlus

powerl :: Primi
powerl = do
  withnop 2
  [a, b] <- getArgumentList
  case (a, b) of
    (Number a1, Number b1) -> maybe noChange (return.Number) (powerN a1 b1)
    (a, Number 1) -> return a
    _ -> noChange

logl :: Primi
logl = do
  withnop 1
  [a] <- getArgumentList
  case a of
    Number 1 -> return (Number 0)
    _ -> noChange
