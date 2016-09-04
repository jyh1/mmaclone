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
  UnpackArith ->
  Packer ->
  (Number -> [LispVal] -> Primi) ->
  Primi
timesOrPlus mp zero unpacker pack merge = do
  (nums, syms) <- fmap (span checkNum) getArgumentList
  let unpacked = map unpackNum nums
      ans = foldl mp zero unpacked
  fmap sortList $ merge ans (totalSimplify unpacker pack syms)

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
timesl = timesOrPlus (*) 1 unpackPower packPower mergeTimes
plusl = timesOrPlus (+) 0 unpackTimes packTimes mergePlus

powerl :: Primi
powerl = do
  withnop 2
  [a, b] <- getArgumentList
  case (a, b) of
    (Number a1, Number b1) -> maybe noChange (return.Number) (powerN a1 b1)
    (_, Number 0) -> return (Number 1)
    (a, Number 1) -> return a
    (List [Atom "Power", n1, n2], b) -> return (List [Atom "Power", n1, List[Atom "Times", b, n2]])
    _ -> noChange

logl :: Primi
logl = do
  withnop 1
  [a] <- getArgumentList
  case a of
    Number 1 -> return (Number 0)
    _ -> noChange

type UnpackArith = LispVal -> (Number, [LispVal])
type Packer = Number -> [LispVal] -> LispVal
unpackTimes :: UnpackArith
unpackTimes (List (Atom "Times" : Number a : res)) = (a, res)
unpackTimes (List (Atom "Times" : res)) = (1, res)
unpackTimes val = (1, [val])

packTimes :: Packer
packTimes 0 _ = Number 0
packTimes 1 [res] = res
packTimes 1 res = List (Atom "Times":res)
packTimes n res = List (Atom "Times" : Number n : res)

unpackPower :: UnpackArith
unpackPower (List [Atom "Power", res, Number a]) = (a, [res])
unpackPower val = (1, [val])

packPower :: Packer
packPower 0 _ = Number 1
packPower 1 res = head res
packPower n res = List [Atom "Power", head res, Number n]

-- simplify :: UnpackArith -> Packer -> Number -> [LispVal] -> [LispVal] -> [LispVal]
-- simplify unpacker pack n res val@(x:xs) =
--   let (n2, res2) = unpacker x
--       simplified = pack n res : totalSimplify unpacker pack val
--   in
--     if res == res2 then
--       simplify unpacker pack (n + n2) res xs
--     else
--       simplified
-- simplify _ pack n res [] = [pack n res]

simplify :: [(Number, [LispVal])] -> [(Number, [LispVal])]
simplify [] = []
simplify [x] = [x]
simplify ((n1, res1):(n2, res2):xs)
  | res1 == res2 = (n1+n2, res1) : simplify xs
  | otherwise = (n1, res1) : simplify ((n2,res2):xs)

totalSimplify :: UnpackArith -> Packer -> [LispVal] -> [LispVal]
totalSimplify unpacker pack xs =
    let unpacked = map unpacker xs
        sorted = sortOn snd unpacked in
      map (uncurry pack) (simplify sorted)
