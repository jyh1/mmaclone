{-#LANGUAGE ExistentialQuantification, RankNTypes #-}
module Data.Number.Number
  (Number(..), powerN, numberEqual,
    equal, less, lessEqual, greater, greaterEqual, unequal,
    isZero, isOne, isDouble, inexactQ, toNumberDouble)
where

import Data.Ratio
import Data.Function(on)
import Data.Maybe
import Control.Monad
-- import Data.Number.Hier
-- Number Type
data Number = Double Double
            | Rational Rational
            | Integer Integer
  deriving(Eq,Ord)

data Unpacker = forall a. Num a =>
  Unpacker (Number -> Maybe (a, a -> Number)) --(a -> a -> a)


unpackInteger (Integer a) = Just (a, Integer)
unpackInteger _ = Nothing

castToInteger (Integer a) = a
castToInteger (Rational a) = round a
castToInteger (Double a) = round a

unpackRational (Integer a) = Just (fromInteger a, Rational)
unpackRational (Rational a) = Just (a, Rational)
unpackRational _ = Nothing

unpackDouble' (Integer a) = fromInteger a
unpackDouble' (Rational a) = fromRational a
unpackDouble' (Double a) = a

unpackDouble a = Just (unpackDouble' a, Double)

toNumberDouble = Double . unpackDouble'

-- | unpacker list
unpackers :: [Unpacker]
unpackers = [Unpacker unpackInteger, Unpacker unpackRational, Unpacker unpackDouble]

instance Show Number where
  show (Integer i) = show i
  show (Double d) = show d
  show (Rational r) = show (numerator r) ++ "/" ++ show (denominator r)

instance Num Number where
  (+) = numberLift (+)
  (-) = numberLift (-)
  (*) = numberLift (*)
  negate = numberMap negate
  abs = numberMap abs
  signum = numberMap signum
  fromInteger = Integer

instance Fractional Number where
  fromRational = Rational
  recip (Integer a) = Rational $ 1 % a
  recip (Rational a) = Rational $ recip a
  recip (Double a) = Double $ recip a

instance Real Number where
  toRational (Integer a) = toRational a
  toRational (Rational a) = a
  toRational (Double a) = toRational a

instance Floating Number where
  pi = Double pi
  exp = doubleMap exp
  log = doubleMap log
  sqrt = doubleMap sqrt
  (**) = doubleLift (**)
  logBase = doubleLift logBase
  sin = doubleMap sin
  cos = doubleMap cos
  tan = doubleMap tan
  asin = doubleMap asin
  acos = doubleMap acos
  atan = doubleMap atan
  sinh = doubleMap sinh
  cosh = doubleMap cosh
  tanh = doubleMap tanh
  asinh = doubleMap asinh
  acosh = doubleMap acosh
  atanh = doubleMap atanh

instance Enum Number where
  toEnum n = Integer (fromIntegral n)
  fromEnum n = fromInteger (castToInteger n)

instance Integral Number where
  quot (Integer a) (Integer b) = Integer $ quot a b
  rem (Integer a) (Integer b) = Integer $ rem a b
  quotRem a b = (quot a b, rem a b)
  toInteger (Integer a) = a

instance RealFrac Number where
  properFraction (Integer n) = (fromInteger n, 0)
  properFraction (Rational a) = fmap Rational (properFraction a)
  properFraction (Double a) = fmap Double (properFraction a)

numberLift' :: Unpacker ->
  (forall a. Num a => a -> a -> a) ->
  Number -> Number -> Maybe Number
numberLift' (Unpacker unpack) f a b = do
  (a', pack) <- unpack a
  (b', _) <- unpack b
  return $ pack (f a' b')
-- | lift a arithmatic function to Number
numberLift ::
  (forall a. Num a => a -> a -> a) ->
  Number -> Number -> Number
numberLift f a b =
  fromJust $ msum attempts
    where
      attempts = [numberLift' unpack f a b | unpack <- unpackers]

numberMap :: (forall a. Num a => a -> a) -> Number -> Number
numberMap f (Integer a) = Integer (f a)
numberMap f (Rational a) = Rational (f a)
numberMap f (Double a) = Double(f a)

doubleMap :: (Double -> Double) -> Number -> Number
doubleMap f n = Double (f (unpackDouble' n))


doubleLift f a b =
  let a' = unpackDouble' a
      b' = unpackDouble' b
  in
    Double $ f a' b'

powerN :: Number -> Number -> Maybe Number
-- double a
powerN (Double a) (Double b) = Just $ Double $ a ** b
powerN (Double a) (Integer b) =Just $ Double $ a ^^ b
powerN (Double a) (Rational b) =Just $ Double $ a ** fromRational b
-- double b
powerN (Integer a) (Double b) = Just $ Double $ fromIntegral a ** b
powerN (Rational a) (Double b) =Just $ Double $ fromRational a ** b
-- integer a
powerN (Integer a) (Integer b)
  | b >= 0 = Just $ Integer $ a ^ b
  | otherwise = Just $ Rational ((1 % a) ^ negate b)
powerN (Integer _) (Rational _) = Nothing
-- integer b
powerN (Rational a) (Integer b)
  | b >= 0 = Just $ Rational $ a ^ b
  | otherwise = Just $ Rational $ (1 / a) ^ negate b
-- rational a
powerN _ _ = Nothing

numberEqual :: Number -> Number -> Bool
numberEqual (Integer a) (Integer b) = a == b
numberEqual (Double a) (Double b) = a == b
numberEqual (Rational a) (Rational b) = a == b
numberEqual a b = ((==) `on` unpackDouble') a b


numberComp :: Number -> Number -> Ordering
numberComp = compare `on` unpackDouble'

compareOnNumber :: (Double -> Double -> Bool) -> Number -> Number -> Bool
compareOnNumber comp =
  comp `on` unpackDouble'

equal = compareOnNumber (==)

less = compareOnNumber (<)

lessEqual = compareOnNumber (<=)
greater = compareOnNumber (>)
greaterEqual = compareOnNumber (>=)

unequal = compareOnNumber (/=)



isZero :: Number -> Bool
isZero = (== 0)
--
isOne :: Number -> Bool
isOne = (== 1)

isDouble :: Number -> Bool
isDouble (Double _) = True
isDouble _ = False

inexactQ :: Number -> Bool
inexactQ = isDouble
