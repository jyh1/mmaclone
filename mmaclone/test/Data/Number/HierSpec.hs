module Data.Number.HierSpec where

import Data.Number.Hier

import Test.Hspec
-- import Test.QuickCheck
-- import Control.Exception(evaluate)

data Number = I Int | D Double
  deriving (Eq)

instance Hier Number where
  rank (I _) = 1
  rank (D _) = 2
  upgrade (I i) = D (fromIntegral i)
  downgrade (D d) = I (truncate d)

plus (I i1) (I i2) = I (i1 + i2)
plus (D d1) (D d2) = D (d1 + d2)

spec :: Spec
spec  = do
  describe "test hier module" $ do
    it "peer eval upgrade" $ do
      peerOpUp plus (I 2) (D 2.3) == D 4.3
    it "peer eval downgrade" $ do
      peerOpDown plus (I 3) (D 2.4) == I 5

main :: IO ()
main = hspec spec
