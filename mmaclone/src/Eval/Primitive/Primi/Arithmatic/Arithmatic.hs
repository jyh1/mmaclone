module Eval.Primitive.Primi.Arithmatic.Arithmatic
        (plusl,timesl,powerl) where
import Data.DataType
import Data.Number.Number
import Eval.Primitive.PrimiType

import Control.Monad
import Control.Monad.Except
import Data.List

numericPolop :: (Number -> [LispVal] -> Result) ->
  ([LispVal] -> LispVal) ->
  ([LispVal] -> Result) ->
  (Number -> Number -> Number) -> [LispVal]
  -> Result
numericPolop _ _ _ _ [] = throwError $ NumArgs "Plus" 0 []
numericPolop _ _ _ _ [a] = return $ Just a
numericPolop merges groupers tagHead op params = do
  let (nums,others) = partition checkNum params
      unpacked = map unpackNum nums
      grouped = map groupers (group others)
  if unpacked /= [] then do
    let ans = foldl1 op unpacked
    merges ans grouped
  else tagHead grouped

mergePlus,mergeTimes :: Number -> [LispVal] -> Result
mergePlus num [] = hasValue (Number num)
mergePlus num xs
  | isZero num = returnWithHead "Plus" xs
  | otherwise = merge "Plus" num xs

mergeTimes num [] = hasValue (Number num)
mergeTimes num xs
  | isZero num = hasValue $ Number zero
  | isOne num = returnWithHead "Times" xs
  | otherwise = merge "Times" num xs

returnWithHead :: String -> [LispVal] -> Result
returnWithHead name xs = hasValue $ List (Atom name : xs)

merge :: String -> Number -> [LispVal] -> Result
merge name num xs = returnWithHead name (Number num : xs)

groupPlus,groupTimes :: [LispVal] -> LispVal
groupPlus [single] = single
groupPlus xs = List [Atom "Times", integer (genericLength xs), head xs]

groupTimes [single] = single
groupTimes xs = List [Atom "Power", head xs, integer (genericLength xs)]
-- --------------------------------------------------
numericBinop :: (Number -> Number -> Maybe Number) ->
  BinaryFun
numericBinop f a b
  | checkNum a && checkNum b =
    let a' = unpackNum a
        b' = unpackNum b in
      return $ fmap Number $ f a' b'
  | otherwise = return Nothing

minus, divide:: BinaryFun
minus (Number a) (Number b) = hasValue (Number $ minusN a b)
minus a b = liftEval minus' a b
  where
    minus' a b = List [Atom "Plus", a, List [Atom "Times", Number $ Integer (-1), b]]

divide (Number a) (Number b) = hasValue (Number $ divideN a b)
divide a b = liftEval divide' a b
  where
    divide' a b = List [Atom "Times", a, List [Atom "Power", b, Number $ Integer (-1)]]

-- modl = numericBinop ((Just.). modN)
powerl = binop "Power" $ numericBinop powerN
-- ----------------------------------------

plusl = numericPolop mergePlus groupPlus (returnWithHead "Plus") plus
timesl = numericPolop mergeTimes groupTimes (returnWithHead "Times") times
