module EvalSpec where

import Eval
import DataType
import Parse

import Test.Hspec
-- import Test.QuickCheck
-- import Control.Exception(evaluate)

testEval = extractValue . (>>= eval)

spec :: Spec
spec  = do
  describe "testEvaluate an LispVal" $ do
    context "non recursive" $ do
      it "adding" $ do
        testEval (readExpr "(+ 1 2 3 4)") == Number 10
      it "subtract" $ do
        testEval (readExpr "(- 3 2 1)") == Number 0
      it "times" $ do
        testEval (readExpr "(* 2 3 4)") == Number 24

    context "recursive" $ do
      it "recursive exp" $ do
        testEval (readExpr "(+ 1 (/ 4 2) (* 2 3))") == Number 9
