module PatternSpec where

import DataType
import Trans
import Number
import Pattern

import Test.Hspec

getExpr = extractValue . readExpr
exp1 = getExpr "1+2+3"
patt1 = getExpr "1+2+x_"
patt2 = getExpr "2+3+_"

spec :: Spec
spec  = do
  describe "pattern mathcing" $
      it "get matched expression" $ do
        getMatch patt1 exp1 `shouldBe` Just [("x", integer 3)]
        getMatch patt2 exp1 `shouldBe` Nothing
