module PatternSpec where

import DataType
import Parse
import Number
import Pattern

import Test.Hspec

getExpr = extractValue . readExpr
exp1 = getExpr "(+ 1 2 3)"
patt1 = getExpr "(+ 1 2 (pattern x (blank)))"
patt2 = getExpr "(+ 2 3 (blank))"

spec :: Spec
spec  = do
  describe "pattern mathcing" $
      it "get matched expression" $ do
        getMatch patt1 exp1 `shouldBe` Just [("x", integer 3)]
        getMatch patt2 exp1 `shouldBe` Nothing
