module EvalSpec where

import Eval
import DataType
import Parse
import Data.Ratio
import Number
import System.IO.Unsafe
import Control.Monad.Except

import Test.Hspec
-- import Test.QuickCheck
-- import Control.Exception(evaluate)

test1 = test . return

test a b =
  let parsed = extractValue $ mapM readExpr a in
    testEval parsed `shouldBe` b

testEval :: [LispVal] -> LispVal
testEval exprs = unsafePerformIO $ do
  env <- nullEnv
  evaled <- runExceptT $ mapM (eval env) exprs
  return (last (extractValue evaled))

true = Bool True
false = Bool False

double :: Double -> LispVal
double = Number . Double

rational = Number . Rational


spec :: Spec
spec  = do
  describe "testEvaluate a LispVal" $ do
    context "number evaluation" $ do
      it "plus" $ do
        test1 "(+ 1 2 3 4)" $ (integer 10)
      it "subtract" $ do
        test1 "(- 3 2.0)" (double 1.0)
      it "divide" $ do
        test1 "(/ 2 3)" (rational $ 2 % 3)
      it "unesscary rational number" $ do
        test1 "(* (/ 3 2) 2)" (integer 3)

      it "power evaluation" $ do
        test1 "(^ 3 -2)" (rational (1 % 9))

--     -- context "head test" $ do
--     --   it "test string" $ do
--     --     test1 "(string? \"sdf\")" Bool True

    context "recursive" $ do
      it "recursive exp" $ do
        test1 "(+ 1 (/ 4 2) (* 2 3))" (integer 9)
      it "recursive" $ do
        test1 "(* 3 2 4 (/ 4 9))" (rational $ 32 % 3)

    context "undefined symbol" $ do
      it "undefined symbol" $ do
        test1 "(xyz (+ 1 2) 3)" $ List [Atom "xyz", (Number . Integer) 3,
                                                        (Number . Integer) 3]
      it "undefined symbol" $ do
        test1 "(* a 3)" $ List [Atom "*", Number $ Integer 3, Atom "a"]

    context "lisp mainpulation" $ do
      context "length" $ do
        it "evaluate length of a lispval" $ do
          test1 "(length (23 3 2 \"sdf\" (/ 3 2)))" $ integer 5
        it "length of an atom value" $ do
          test1 "(length (- 2 3))" $ integer 0
      context "part" $ do
        it "part of a list,index from 0" $ do
          test1 "(part (1 2 3 4) 3)" $ integer 4
        it "arbitrary nest" $ do
          test1 "(part (1 2 (2 (3) 3) (+ 2 3)) (2 1 0))" $ integer 3
      context "car" $ do
        it "return first element" $ do
          test1 "(car (1 2 3))" (integer 1)
      context "cdr" $ do
        it "return the rest elements" $ do
          test1 "(cdr (1 2))" (List [integer 2])
--
    context "comparing function" $ do
      context "compare number" $ do
        it "less" $ do
          test1 "(< 3 2)" false
        it "less" $ do
          test1 "(< 2.0 3)" true
        it "equal" $ do
          test1 "(== 2 2.0)" true
        it "equal" $ do
          test1 "(== 2 4.0)" false


      context "compare string" $ do
        it "less" $ do
          test1 "(< \"ab\" \"bc\")" true
        it "less" $ do
          test1 "(< \"bc\" \"ab\")" false


    context "logic function" $ do
      context "&&" $ do
        it "case #t #t" $ do
          test1 "(&& #t #t)" true
        it "case #f #f" $ do
          test1 "(&& #f #f)" false
      context "||" $ do
        it "case #t #f" $ do
          test1 "(|| #t #f)" true
        -- it "case #f #f" $ do
          test1 "(|| #f #f)" false
      context "!" $ do
        it "case #t" $ do
          test1 "(! #t)" false

  context "eval with context" $ do
    it "single value" $ do
      test ["(set a 3)", "a"] $ integer 3
      test ["(set a 3)", "(set (b 2) 4)","(+ a (b 2))"] (integer 7)
    it "factorial" $ do
      test ["(setDelayed (fact n_) (* n (fact (- n 1))))",
            "(set (fact 0) 1)","(fact 10)"] (integer 3628800)
    it "fibonacci" $ do
      test
        ["(setDelayed (fib n_) (set (fib n) (+ (fib (- n 1)) (fib (- n 2)))))",
          "(set (fib 1) 1)", "(set (fib 2) 1)", "(fib 40)"]
        (integer 102334155)
