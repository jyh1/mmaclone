module Eval.Patt.PatternSpec where

import Data.DataType
import Parser.Trans
import Data.Number.Number
import Eval.Patt.Pattern

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
