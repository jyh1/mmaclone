module EvalSpec where

import Eval
import DataType
import Parse

import Test.Hspec
import Test.QuickCheck
import Control.Exception(evaluate)

spec :: Spec
spec  = do
  describe "evaluate an LispVal" $ do
    context "non recursive" $ do
      it "adding" $ do
        eval (readExpr "(+ 1 2 3 4)") == Number 10
      it "subtract" $ do
        eval (readExpr "(- 3 2 1)") == Number 0
      it "times" $ do
        eval (readExpr "(* 2 3 4)") == Number 24

    context "recursive" $ do
      it "recursive exp" $ do
        eval (readExpr "(+ 1 (/ 4 2) (* 2 3))") == Number 9
