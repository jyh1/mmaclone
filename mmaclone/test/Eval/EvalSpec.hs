module Eval.EvalSpec where

import Eval.Eval
import Data.DataType
import Parser.Trans
import Data.Ratio
import Data.Number.Number
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

true = toBool True
false = toBool False

-- double :: Double -> LispVal
-- double = Number . Double

rational = Number . Rational


spec :: Spec
spec  = do
  describe "testEvaluate a LispVal" $ do
    context "number evaluation" $ do
      it "plus" $ do
        test1 "1+2+3+4" $ (integer 10)
      it "subtract" $ do
        test1 "3-2.0" (double 1.0)
      it "divide" $ do
        test1 "2/3" (rational $ 2 % 3)
      it "unesscary rational number" $ do
        test1 "3/2*2" (integer 3)

      it "power evaluation" $ do
        test1 "3^(-2)" (rational (1 % 9))

--     -- context "head test" $ do
--     --   it "test string" $ do
--     --     test1 "(string? \"sdf\")" Bool True

    context "recursive" $ do
      it "recursive exp" $ do
        test1 "1+4/2+2*3" (integer 9)
      it "recursive" $ do
        test1 "3 2 4 4/9" (rational $ 32 % 3)

    context "undefined symbol" $ do
      it "undefined symbol" $ do
        test1 "xyz[1+2,3]" $ List [Atom "xyz", (Number . Integer) 3,
                                                        (Number . Integer) 3]
        test1 "a 3" $ List [Atom "Times", Number $ Integer 3, Atom "a"]
      it "merge same head" $ do
        test1 "x*x*2" $ List [Atom "Times", integer 2, List [Atom "Power", Atom "x",integer 2]]

    context "sequence" $ do
      it "expand sequence" $ do
        test1 "f[x,Sequence[y,y]]" $ List [Atom "f", Atom "x", Atom "y", Atom "y"]

    context "lisp mainpulation" $ do
      context "length" $ do
        it "evaluate length of a lispval" $ do
          test1 "Length@{23, 3 ,2, \"sdf\" ,3/2}" $ integer 5
        it "length of an atom value" $ do
          test1 "Length@(1-2)" $ integer 0

      context "part" $ do
        it "part of a list,index from 0" $ do
          test1 "{1,2,3,4}[[4]]" $ integer 4
        it "arbitrary nest" $ do
          test1 "{{1, 2, 3}, {2, 3}, 3, 4}[[{2, 1}, {1, 2}]]" $ list [list [integer 2,integer 3],list [integer 1,integer 2]]
          test1 "{{1,2},{3,4,5}}[[2,1]]" $ integer 3
          test1 "(1+2x+3)[[1]]" $ integer 4

      context "car" $ do
        it "return first element" $ do
          test1 "car[{1,2,3}]" (Atom "List")
      context "cdr" $ do
        it "return the rest elements" $ do
          test1 "cdr[{1,2}]" (List [integer 1, integer 2])
--
    -- context "comparing function" $ do
    --   context "compare number" $ do
    --     it "less" $ do
    --       test1 "(< 3 2)" false
    --     it "less" $ do
    --       test1 "(< 2.0 3)" true
    --     it "equal" $ do
    --       test1 "(== 2 2.0)" false
    --     it "equal" $ do
    --       test1 "(== 2 4.0)" false
    --
    --
    --   context "compare string" $ do
    --     it "less" $ do
    --       test1 "(< \"ab\" \"bc\")" true
    --     it "less" $ do
    --       test1 "(< \"bc\" \"ab\")" false


    context "logic function" $ do
      context "&&" $ do
        it "case #t #t" $ do
          test1 "True&&True" true
        it "case #f #f" $ do
          test1 "False && False" false
      context "||" $ do
        it "case #t #f" $ do
          test1 "True||False" true
        -- it "case #f #f" $ do
          test1 "False || False" false
      context "!" $ do
        it "case #t" $ do
          test1 "!True" false

  context "eval with context" $ do
    it "single value" $ do
      test ["a=3", "a"] $ integer 3
      test ["a=3", "b[2]=4","a+b[2]"] (integer 7)
    it "factorial" $ do
      test ["fact[n_]:=n fact[n-1]",
            "fact[0]=1","fact[10]"] (integer 3628800)
    it "fibonacci" $ do
      test
        ["fib[n_]:=fib[n]=fib[n-1]+fib[n-2]",
          "fib[1]=1;fib[2]=1;", "fib[40]"]
        (integer 102334155)
