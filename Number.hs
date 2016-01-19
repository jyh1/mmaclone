module Number where

import Data.Ratio
import Data.Function(on)
import Hier
-- Number Type
data Number = Double Double
            | Rational Rational
            | Integer Integer
  deriving(Eq)

instance Show Number where
  show (Integer i) = show i
  show (Double d) = show d
  show (Rational r) = show r

instance Ord Number where
  (<=) = (<=) `on` toDouble

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

plus :: Number -> Number -> Number
plus = peerOpUp plus'
  where plus' (Integer i1) (Integer i2) = Integer $ i1 + i2
        plus' (Rational r1) (Rational r2) = Rational (r1 + r2)
        plus' (Double d1) (Double d2) = Double $ d1 + d2

-- minus :: Number -> Number -> Number
-- minus = peerOpUp minus'
--   where minus' (Integer i1) (Integer i2) = Integer $ i1 - i2
--         minus' (Rational r1) (Rational r2) = Rational $ r1 - r2
--         minus' (Double d1) (Double d2) = Double $ d1 - d2

times :: Number -> Number -> Number
times = peerOpUp times'
  where times' (Integer i1) (Integer i2) = Integer $ i1 * i2
        times' (Rational r1) (Rational r2) = Rational $ r1 * r2
        times' (Double d1) (Double d2) = Double $ d1 * d2

-- divide :: Number -> Number -> Number
-- divide = peerOpUp divide'
--   where divide' (Integer i1) (Integer i2)
--           | i1 `mod` i2 == 0= Integer $ i1 `div` i2
--           | otherwise = Rational $ i1 % i2
--         divide' (Rational r1) (Rational r2) = Rational $ r1 / r2
--         divide' (Double d1) (Double d2) = Double $ d1 / d2

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
