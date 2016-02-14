module Eval.EvalSpec where

import Data.Environment.Environment
import Eval.Eval
import Data.DataType hiding (list)
import Parser.Trans
import Test
import Data.Ratio
import Data.Number.Number hiding(plus,times,one,less,lessEqual,greater,greaterEqual,equal)
import System.IO.Unsafe
import Control.Monad.Except

import Test.Hspec
-- import Test.QuickCheck
-- import Control.Exception(evaluate)


test1 = test . return

test a b =
  let parsed = extractValue $ mapM readExpr a in
    testEval parsed `shouldBe` b

test2 a b = test1 a (readVal b)

testEval :: [LispVal] -> LispVal
testEval exprs = unsafePerformIO $ do
  env <- nullEnv
  evaled <- runExceptT $ mapM (eval env) exprs
  return (last (extractValue evaled))


-- double :: Double -> LispVal
-- double = Number . Double



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

    context "Eval.Primitive.Primi.List.Part" $ do
      context "part" $ do
        it "part of a list,index from 0" $ do
          test1 "{1,2,3,4}[[4]]" $ integer 4
        it "arbitrary nest" $ do
          test1 "{{1, 2, 3}, {2, 3}, 3, 4}[[{2, 1}, {1, 2}]]" $ list [list [integer 2,integer 3],list [integer 1,integer 2]]
          test1 "{{1,2},{3,4,5}}[[2,1]]" $ integer 3
          test1 "(1+2x+3)[[1]]" $ integer 4

      context "Eval.Primitive.Primi.List.Cons" $ do
        context "Range" $ do
          it "different argumens" $ do
            test1 "Range[1,2,1]" $ list [one,two]
            test1 "Range[3]" $ list [one,two,three]
            test1 "Range[1,1.1,0.1]" $ list [double 1, double 1.1]
            test1 "Range[1,1+1/2,1/2]" $ list [integer 1,rational (3%2)]

      context "Eval.Primitive.Primi.List.Elem" $ do
        context "car" $ do
          it "return first element" $ do
            test1 "car[{1,2,3}]" (Atom "List")
        context "cdr" $ do
          it "return the rest elements" $ do
            test1 "cdr[{1,2}]" (List [integer 1, integer 2])
        context "cons" $ do
          it "cons" $ do
            test1 "cons[1,{1,2}]" (List [one,Atom "List",one,two])

    context "Eval.Primitive.Primi.List.Map" $ do
      context "map" $ do
        it "level 1" $ do
          test1 "P/@{1,2,3}" (readVal "{P[1],P[2],P[3]}")
          test1 "Map[f,{1,2,3}]" (readVal "{f[1],f[2],f[3]}")
        it "other level" $ do
          test1 "Map[f,{1,2,{3}},2]" (readVal "{f[1],f[2],f[{f[3]}]}")
          test1 "Map[f,{1,2,{3}},{2}]" (readVal "{1,2,{f[3]}}")
          test1 "Map[f,{1,{{2},3}},{2,3}]" (readVal "{1,{f[{f[2]}],f[3]}}")

      context "apply" $ do
        it "level 0" $ do
          test1 "f@@{1,2,3}" (readVal "f[1,2,3]")
          test1 "Apply[f,{1,2,3},{0}]" (readVal "f[1,2,3]")
        it "other level" $ do
          test1 "Apply[f,{1,{2,{3}}},2]" (readVal "{1,f[2,f[3]]}")
          test1 "f@@@{1,{2},3}" (readVal "{1,f[2],3}")
          test1 "Apply[f,{1,{2,{3}}},{2}]" (readVal "{1,{2,f[3]}}")
          test1 "Apply[f,{1,{2,{3}}},{0,1}]" (readVal "f[1,f[2,{3}]]")


  --
    context "Eval.Primitive.Primi.Compare" $ do
      context "compare number" $ do
        it "compare" $ do
          test1 "Less[1,2.1,3,4]" true
          test1 "Less[1/2,3,2]" false
        it "inequality" $ do
          test1 "1==1==a" (readVal "1==a")
          test1 "1>2<3<a<2<3" false
          test1 "a<b<2<3<4>=2<=2.0>=1.0" (readVal "a<b<2")
          test1 "a<b<2>3.1" false
          test1 "1!=2!=3" true
          test1 "1!=1==3" false

    context "Eval.Logic.Logic" $ do
      context "&&" $ do
        it "True" $ do
          test1 "True&&True" true
        it "False" $ do
          test1 "False && False" false
          test2 "a&&b&&True&&1!=1.0" "False"
          test2 "a&&1==1&&2==2" "a"
      context "||" $ do
        it "case #t #f" $ do
          test1 "True||False||a" true
          test2 "b||False || False||a" "b||a"
          test2 "a||1==2||2==1.0||2!=2.0" "a"
      context "!" $ do
        it "case #t" $ do
          test1 "!True" false

  context "Eval.Control.Branch" $ do
    context "If" $ do
      it "If expression" $ do
        test1 "If[True,1,2]" (readVal "1")
        test1 "If[False,1,2]" (readVal "2")
        test1 "If[1,2,3,4]" (readVal "4")
  context "eval with context" $ do
    it "single value" $ do
      test ["a=3", "a"] $ integer 3
      test ["a=3", "b[2]=4","a+b[2]"] (integer 7)
    it "factorial" $ do
      test ["fact[n_]:=n fact[n-1]",
            "fact[0]=1","1==2&&fact[1000000]","fact[10]"] (integer 3628800)
    it "fibonacci" $ do
      test
        ["fib[n_]:=fib[n]=fib[n-1]+fib[n-2]",
          "fib[1]=1;fib[2]=1;", "fib[40]"]
        (integer 102334155)

    it "pattern matching" $ do
      test
        ["a[0]=1","a[0.0]=1","a[0]","a[0.0]"] (integer 1)


main = hspec spec
