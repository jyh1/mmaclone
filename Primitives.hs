module Primitives(primitives) where

import DataType
-- import Hier
import Number
-- import Pattern

import Control.Monad
import Control.Monad.Except
-- import Data.Ratio
-- import Data.Maybe(fromMaybe)
import Data.List(partition, genericLength, genericIndex,group)
-- import qualified Data.Map.Strict as M

primitives :: [(String,[LispVal] -> Result)]
primitives = [
              -- numeric function
              ("+", numericPolop mergePlus groupPlus (returnWithHead "+") plus),
              ("-", binop minus),
              ("*", numericPolop mergeTimes groupTimes (returnWithHead "*") times),
              ("/", binop divide),
              ("^", binop powerl),
              -- list mainpulation
              ("car", sinop car),
              ("cdr", sinop cdr),
              ("length", sinop len),
              ("part", binop part),
              -- ("")
              -- comparation
              ("<", binop lessThan),
              ("<=", binop lessEqual),
              (">", binop greaterThan),
              (">=", binop greaterEqual),
              ("==", binop equal),
              -- ("symbol?", testHead symbolQ),
              -- ("string?", testHead stringQ),
              -- ("number?", testHead numberQ),
              -- ("quote", quoted)
              -- ("quoteient", numericBinop quot),
              ("&&", binop andl),
              ("||", binop orl),
              ("!", sinop notl)
            ]

-- evaluation helper function
binop :: BinaryFun -> [LispVal]
  -> Result
binop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
binop op [a, b] = op a b
binop _ vals = throwError $ NumArgs 2 vals

sinop :: SingleFun ->
          [LispVal] -> Result
sinop op [x] = op x
sinop _ vals  = throwError $ NumArgs 1 vals

liftEval :: (LispVal -> LispVal -> LispVal) ->
              BinaryFun
liftEval f a b = return $ Just (f a b)

internalBoolOp :: (Bool ->Bool -> Bool) -> Result -> Result -> Result
internalBoolOp f =
  liftM2 f''
    where
      f'' = liftM2 f'
      f' (Bool a) (Bool b) = Bool $ f a b

internalAnd = internalBoolOp (&&)
internalOr = internalBoolOp (||)

internalNot :: Result -> Result
internalNot=
  liftM f''
    where f'' = liftM f'
          f' (Bool a) = Bool $ not a

-- Number evaluation
numericPolop :: (Number -> [LispVal] -> Result) ->
  ([LispVal] -> LispVal) ->
  ([LispVal] -> Result) ->
  (Number -> Number -> Number) -> [LispVal]
  -> Result
numericPolop _ _ _ _ [] = throwError $ NumArgs 0 []
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
  | isZero num = returnWithHead "+" xs
  | otherwise = merge "+" num xs

mergeTimes num [] = hasValue (Number num)
mergeTimes num xs
  | isZero num = hasValue $ Number zero
  | isOne num = returnWithHead "*" xs
  | otherwise = merge "*" num xs

returnWithHead :: String -> [LispVal] -> Result
returnWithHead name xs = hasValue $ List (Atom name : xs)

merge :: String -> Number -> [LispVal] -> Result
merge name num xs = returnWithHead name (Number num : xs)

groupPlus,groupTimes :: [LispVal] -> LispVal
groupPlus [single] = single
groupPlus xs = List [Atom "*", integer (genericLength xs), head xs]

groupTimes [single] = single
groupTimes xs = List [Atom "^", head xs, integer (genericLength xs)]
-- --------------------------------------------------
numericBinop :: (Number -> Number -> Maybe Number) ->
  BinaryFun
numericBinop f a b
  | checkNum a && checkNum b =
    let a' = unpackNum a
        b' = unpackNum b in
      return $ fmap Number $ f a' b'
  | otherwise = return Nothing

minus, divide, powerl:: BinaryFun
minus (Number a) (Number b) = hasValue (Number $ minusN a b)
minus a b = liftEval minus' a b
  where
    minus' a b = List [Atom "+", a, List [Atom "*", Number $ Integer (-1), b]]

divide (Number a) (Number b) = hasValue (Number $ divideN a b)
divide a b = liftEval divide' a b
  where
    divide' a b = List [Atom "*", a, List [Atom "^", b, Number $ Integer (-1)]]

-- modl = numericBinop ((Just.). modN)
powerl = numericBinop powerN
-- ----------------------------------------



-- head test functions
testHead :: (LispVal -> Bool) -> [LispVal] -> ThrowsError LispVal
testHead test vals = return (Bool (all test vals))

symbolQ , stringQ, numberQ :: LispVal -> Bool

symbolQ (Atom _) = True
symbolQ _ = False

stringQ (String _) = True
stringQ _ = False

numberQ (Number _) = True
numberQ _ = False


-- list manipulation functions
len :: SingleFun
len x = return $ Just $ len' x
        where
          len' (List x) = integer $ genericLength x
          len' _ = integer 0

part :: BinaryFun
part x nv@(Number (Integer n)) = part x (List [nv])
part val (List []) = hasValue val
part val@(List x) (List (nv@(Number (Integer n)) : ns)) =
  if genericLength x <= n then throwError (PartError val nv)
                   else part (genericIndex x n) (List ns)
part x n = throwError (PartError x n)

car ,cdr :: SingleFun
car (List []) = throwError (Default "car::empty list")
car (List (x:_)) = hasValue x
car _ = noChange

cdr (List []) = throwError (Default "cdr:: empty list")
cdr (List (_:xs)) = hasValue (List xs)
cdr _ = noChange
-- ------------------------------------------


-- compare function
unpackCompare :: LispVal -> LispVal ->
                  Unpacker -> ThrowsError Ordering
unpackCompare a b (Unpacker unpack) = do
  unpacka <- unpack a
  unpackb <- unpack b
  return $  compare unpacka unpackb

getCompareResult :: LispVal -> LispVal -> ThrowsError Ordering
getCompareResult a b =
  sumError $ map (unpackCompare a b) unpackers

getBoolResult :: Ordering -> BinaryFun
getBoolResult e a b =
  let boolRes x = Just . Bool $ x == e in
    liftM boolRes (getCompareResult a b)
      `catchError` const noChange

equal :: BinaryFun
equal = getBoolResult EQ

lessThan :: BinaryFun
lessThan = getBoolResult LT

lessEqual,greaterThan,greaterEqual :: BinaryFun
lessEqual a b = internalOr (equal a b) (lessThan a b)
greaterThan = (internalNot.). lessEqual
greaterEqual = (internalNot.). lessThan
-- ----------------------------

-- logic function
logic :: (Bool -> Bool -> Bool) -> BinaryFun
logic f (Bool a) (Bool b) = hasValue $ Bool (a `f` b)
logic _ _ _ = noChange

andl, orl :: BinaryFun
andl = logic (&&)
orl = logic (||)

notl :: SingleFun
notl (Bool a) = hasValue $ Bool $ not a
notl _ = noChange
-- --------------------------------
