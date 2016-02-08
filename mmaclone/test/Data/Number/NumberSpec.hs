module Data.Number.NumberSpec where

import Data.Number.Number
import Data.Ratio
import Test.Hspec

i1 = 3
i2 = 4
i3 = -4

int1 = Integer i1
int2 = Integer i2
int3 = Integer i3

d1 =  3.0
d2 =  2.0
d3 = -4.0

dou1 = Double d1
dou2 = Double d2
dou3 = Double d3

r1 = 1 % 3
r2 = 3 % 5
r3 = negate 4 % 7

ra1 = Rational r1
ra2 = Rational r2
ra3 = Rational r3

int = Just . Integer
dou = Just . Double
ra = Just . Rational

spec :: Spec
spec = do
  describe "power evaluation" $ do
    context "raise int to int" $ do
      it "positive exponent" $ do
        powerN int1 int2 == int (i1 ^ i2)
      it "negative exponet" $ do
        powerN int1 int3 == ra ((1 % i1) ^ (negate i3))

    context "rational number" $ do
      it "rational base to int exp" $ do
        powerN ra1 int1 == ra (r1 ^ i1)
      it "negative exponent" $ do
        powerN ra1 int3 == ra ((1 / r1) ^ negate i3)

    context "rational exponent" $ do
      it "return nothing" $ do
        powerN int1 ra1 == Nothing

    context "double involved" $ do
      it "double exponent" $ do
        powerN ra1 dou1 == dou (fromRational r1 ** d1)
      it "double base" $ do
        powerN dou1 int3 == dou (d1 ** fromIntegral i3)

  -- describe ""
