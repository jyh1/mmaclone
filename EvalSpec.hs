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
test a b = (testEval . readExpr) a == b

true = Bool True
false = Bool False

spec :: Spec
spec  = do
  describe "testEvaluate a LispVal" $ do
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
        test "(^ 3 -2)" (Number $ Rational (1 % 9))

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

    context "lisp mainpulation" $ do
      context "length" $ do
        it "evaluate length of a lispval" $ do
          test "(length (23 3 2 \"sdf\" (/ 3 2)))" $ integer 5
        it "length of an atom value" $ do
          test "(length (- 2 3))" $ integer 0
      context "part" $ do
        it "part of a list,index from 0" $ do
          test "(part (1 2 3 4) 3)" $ integer 4
        it "arbitrary nest" $ do
          test "(part (1 2 (2 (3) 3) (+ 2 3)) (2 1 0))" $ integer 3
      context "car" $ do
        it "return first element" $ do
          test "(car (1 2 3))" (integer 1)
      context "cdr" $ do
        it "return the rest elements" $ do
          test "(cdr (1 2))" (List [integer 2])

    context "comparing function" $ do
      context "compare number" $ do
        it "less" $ do
          test "(< 3 2)" false
        it "less" $ do
          test "(< 2.0 3)" true
        it "equal" $ do
          test "(== 2 2.0)" true
        it "equal" $ do
          test "(== 2 4.0)" false


      context "compare string" $ do
        it "less" $ do
          test "(< \"ab\" \"bc\")" true
        it "less" $ do
          test "(< \"bc\" \"ab\")" false


    context "logic function" $ do
      context "&&" $ do
        it "case #t #t" $ do
          test "(&& #t #t)" true
        it "case #f #f" $ do
          test "(&& #f #f)" false
      context "||" $ do
        it "case #t #f" $ do
          test "(|| #t #f)" true
        it "case #f #f" $ do
          test "(|| #f #f)" false
      context "!" $ do
        it "case #t" $ do
          test "(! #t)" false
