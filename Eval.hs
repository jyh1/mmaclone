module Eval
    (
    eval
    ) where

import DataType
import Hier
import Number

import Control.Monad
import Control.Monad.Except
import Data.Ratio
import Data.Maybe(fromMaybe)
import Data.List(partition)
-- import Control.Monad.Trans.Maybe

eval :: LispVal -> ThrowsError LispVal
eval val = do
  x1 <- eval' val
  if x1 == val then return x1 else eval x1
  -- let ans = sequence $ iterate (>>= eval') (return val)
  --     fixed (x1 : (x2 : xs))
  --       | x1 == x2 = x1
  --       | otherwise = x2 in
  --       -- | otherwise = fixed (x2 : xs) in
  --   liftM fixed ans
  --   -- undefined

eval' :: LispVal -> ThrowsError LispVal
eval' (List (v:vs)) = do
  headE <- eval v
  args <- mapM eval vs
  let old = List (headE : args)
      getFName (Atom f) = Just f
      getFName _ = Nothing
  let fun = do
        name <- getFName headE
        lookup name primitives
  case fun of
    Just f -> liftM (fromMaybe old) (f args)
    Nothing -> return old

eval' n@(Number (Rational r))
  | denominator r == 1 = return (Number $ Integer $ numerator r)
  | otherwise = return n

eval' x = return x

primitives :: [(String,[LispVal] -> ThrowsError (Maybe LispVal))]
primitives = [
              ("+", numericPolop "+" plus),
              -- ("-", binop minus),
              ("*", numericPolop "*" times),
              -- ("/", binop divide),
              -- ("mod", binop modl),
              ("^", binop powerl)
              -- ("symbol?", testHead symbolQ),
              -- ("string?", testHead stringQ),
              -- ("number?", testHead numberQ),
              -- ("quote", quoted)
              -- ("quoteient", numericBinop quot),
              -- ("remainder", numericBinop rem)
            ]
-- quote
quoted :: [LispVal] -> ThrowsError LispVal
quoted x = return $ List (Atom "quote" : x)

-- evaluation helper function
binop :: (LispVal -> LispVal -> ThrowsError (Maybe LispVal)) -> [LispVal]
  -> ThrowsError (Maybe LispVal)
binop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
binop op [a, b] = op a b
binop _ vals = throwError $ NumArgs 2 vals

liftEval :: (LispVal -> LispVal -> LispVal) ->
              LispVal -> LispVal -> ThrowsError (Maybe LispVal)
liftEval f a b = return $ Just (f a b)

---------------------------------------------------

-- Number evaluation
numericPolop :: String -> (Number -> Number -> Number) -> [LispVal]
  -> ThrowsError (Maybe LispVal)
numericPolop _ _ [a] = return $ Just a
numericPolop name op params = do
  let (nums,others) = partition checkNum params
      unpacked = map unpackNum nums
  let ans = foldl1 op unpacked
  return . Just $ case others of
          [] -> Number ans
          _ -> List $ Atom name : (Number ans : others)

checkNum :: LispVal -> Bool
checkNum (Number _) = True
checkNum _ = False

unpackNum :: LispVal -> Number
unpackNum (Number n) = n

numericBinop :: (Number -> Number -> Maybe Number) ->
  LispVal -> LispVal -> ThrowsError (Maybe LispVal)
numericBinop f a b
  | checkNum a && checkNum b =
    let a' = unpackNum a
        b' = unpackNum b in
      return $ fmap Number $ f a' b'
  | otherwise = return Nothing

minus, divide, powerl, modl :: LispVal -> LispVal -> ThrowsError (Maybe LispVal)
minus = liftEval minus'
  where
    minus' a b = List [Atom "+", a, List [Atom "*", Number $ Integer (-1), b]]
divide = liftEval divide'
  where
    divide' a b = List [Atom "*", a, List [Atom "^", b, Number $ Integer (-1)]]

modl = numericBinop ((Just.). modN)
powerl = numericBinop powerN
-- ----------------------------------------



-- head test functions
testHead :: (LispVal -> Bool) -> [LispVal] -> ThrowsError LispVal
testHead test vals = return $ (Bool (all test vals))

symbolQ , stringQ, numberQ :: LispVal -> Bool

symbolQ (Atom _) = True
symbolQ _ = False

stringQ (String _) = True
stringQ _ = False

numberQ (Number _) = True
numberQ _ = False
