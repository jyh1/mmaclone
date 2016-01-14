module EvalSpec where

import Eval
import DataType
import Parse
import Data.Ratio
import Number

import Test.Hspec
-- import Test.QuickCheck
-- import Control.Exception(evaluate)

testEval = extractValue . (>>= eval)
test a b = testEval a == b
spec :: Spec
spec  = do
  describe "testEvaluate an LispVal" $ do
    context "number evaluation" $ do
      it "plus" $ do
        testEval (readExpr "(+ 1 2 3 4)") == (Number . Integer) 10
      it "subtract" $ do
        testEval (readExpr "(- 3 2.0)") == (Number . Double) 1.0
      it "divide" $ do
        testEval (readExpr "(/ 2 3)") == (Number . Rational) (2 % 3)
      it "unesscary rational number" $ do
        testEval (readExpr "(* (/ 3 2) 2)") == (Number . Integer) 3

      it "power evaluation" $ do
        test (readExpr "(^ 3 -2)") (Number $ Rational (1 % 9))

    -- context "head test" $ do
    --   it "test string" $ do
    --     testEval (readExpr "(string? \"sdf\")") == Bool True

    context "recursive" $ do
      it "recursive exp" $ do
        testEval (readExpr "(+ 1 (/ 4 2) (* 2 3))") == (Number . Integer) 9
      it "recursive" $ do
        testEval (readExpr "(* 3 2 4 (/ 4 9))") == (Number . Rational) (32 % 3)

    context "undefined symbol" $ do
      it "undefined symbol" $ do
        testEval (readExpr "(xyz (+ 1 2) 3)") == List [Atom "xyz", (Number . Integer) 3,
                                                        (Number . Integer) 3]
      it "undefined symbol" $ do
        testEval (readExpr "(* a 3)") == List [Atom "*", Number $ Integer 3, Atom "a"]
