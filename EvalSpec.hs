module EvalSpec where

import Eval
import DataType
import Parse
import Data.Ratio

import Test.Hspec
-- import Test.QuickCheck
-- import Control.Exception(evaluate)

testEval = extractValue . (>>= eval)

spec :: Spec
spec  = do
  describe "testEvaluate an LispVal" $ do
    context "number evaluation" $ do
      it "adding" $ do
        testEval (readExpr "(+ 1 2 3 4)") == (Number . Integer) 10
      it "subtract" $ do
        testEval (readExpr "(- 3 2.0 1)") == (Number . Double) 0.0
      it "times" $ do
        testEval (readExpr "(/ 2 3 4)") == (Number . Rational) (1 % 6)
      it "unesscary rational number" $ do
        testEval (readExpr "(* (/ 3 2) 2)") == (Number . Integer) 3

    context "head test" $ do
      it "test string" $ do
        testEval (readExpr "(string? \"sdf\")") == Bool True

    context "recursive" $ do
      it "recursive exp" $ do
        testEval (readExpr "(+ 1 (/ 4 2) (* 2 3))") == (Number . Integer) 9
