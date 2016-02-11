module Data.Number.Number where

import Data.Ratio
import Data.Function(on)
import Data.Number.Hier
-- Number Type
data Number = Double Double
            | Rational Rational
            | Integer Integer
  deriving(Eq,Ord)

instance Show Number where
  show (Integer i) = show i
  show (Double d) = show d
  show (Rational r) = show (numerator r) ++ "/" ++ show (denominator r)

-- instance Eq Number where
  -- (==) = (==) `on` toDouble

-- instance Ord Number where
--   (<=) = (<=) `on` toDouble

instance Hier Number where
  rank (Integer _) = 1
  rank (Rational _) = 2
  rank (Double _) = 3

  upgrade (Integer i) = Rational $ i % 1
  upgrade (Rational r) = Double $ fromRational r

  downgrade (Double d) = Rational $ toRational d
  downgrade (Rational r) = Integer (truncate $ fromRational r)

toDouble :: Number -> Double
toDouble (Integer n) = fromIntegral n
toDouble (Rational r) = fromRational r
toDouble (Double x) = x

toNumberDouble :: Number -> Number
toNumberDouble = Double . toDouble

plus :: Number -> Number -> Number
plus = peerOpUp plus'
  where plus' (Integer i1) (Integer i2) = Integer $ i1 + i2
        plus' (Rational r1) (Rational r2) = Rational (r1 + r2)
        plus' (Double d1) (Double d2) = Double $ d1 + d2

minusN :: Number -> Number -> Number
minusN = peerOpUp minus'
  where minus' (Integer i1) (Integer i2) = Integer $ i1 - i2
        minus' (Rational r1) (Rational r2) = Rational $ r1 - r2
        minus' (Double d1) (Double d2) = Double $ d1 - d2

times :: Number -> Number -> Number
times = peerOpUp times'
  where times' (Integer i1) (Integer i2) = Integer $ i1 * i2
        times' (Rational r1) (Rational r2) = Rational $ r1 * r2
        times' (Double d1) (Double d2) = Double $ d1 * d2

divideN :: Number -> Number -> Number
divideN = peerOpUp divide'
  where divide' (Integer i1) (Integer i2)
          | i1 `mod` i2 == 0= Integer $ i1 `div` i2
          | otherwise = Rational $ i1 % i2
        divide' (Rational r1) (Rational r2) = Rational $ r1 / r2
        divide' (Double d1) (Double d2) = Double $ d1 / d2

-- modN :: Number -> Number -> Number
-- modN = peerOpUp modN'
--   where modN' (Integer i1) (Integer i2) = Integer $ i1 `mod` i2
--         modN' (Double d1) (Double d2) = Double (d1 - (fromIntegral . truncate) (d1 / d2) * d2)

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
-- quoteientN :: Number -> Number -> Number
-- ---------------------------------------
numberEqual :: Number -> Number -> Bool
numberEqual (Integer a) (Integer b) = a == b
numberEqual (Double a) (Double b) = a == b
numberEqual (Rational a) (Rational b) = a == b
numberEqual a b = ((==) `on` toDouble) a b


numberComp :: Number -> Number -> Ordering
numberComp = compare `on` toDouble

isZero :: Number -> Bool
isZero = (== zero)

isOne :: Number -> Bool
isOne = (== Integer 1)

isDouble :: Number -> Bool
isDouble (Double _) = True
isDouble _ = False

inexactQ :: Number -> Bool
inexactQ = isDouble

numberTrunc :: (Integral a) => Number -> a
numberTrunc (Integer n) = fromIntegral n
numberTrunc (Rational d) = truncate d
numberTrunc (Double d) = truncate d

zero :: Number
zero = Integer 0

one :: Number
one = Integer 1

negateN, inverseN :: Number -> Number
negateN (Integer a) = Integer (-a)
negateN (Double a) = Double (-a)
negateN (Rational a) = Rational (-a)

inverseN (Integer a) = Rational (1 % a)
inverseN (Rational a) = Rational (1 / a)
inverseN (Double a) = Double (1 / a)
