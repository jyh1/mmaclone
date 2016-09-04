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
import Control.Monad.Trans.State

import Test.Hspec
-- import Test.QuickCheck
-- import Control.Exception(evaluate)


test1 = test . return

test a b =
  let parsed = extractValue $ mapM readExpr a in
    testEval parsed `shouldBe` b

test2 a b = test1 a (readVal b)

test3 a b = test a (readVal b)

testEval :: [LispVal] -> LispVal
testEval exprs =
  let expr = mapM eval exprs
      evaled = runExceptT $ evalStateT expr initialState in
    last (extractValue (unsafePerformIO evaled))

-- double :: Double -> LispVal
-- double = Number . Double

spec :: Spec
spec  = do
  describe "testEvaluate a LispVal" $ do
    context "number evaluation" $ do
      it "plus" $ do
        test1 "1+2+3+4" $ (integer 10)
        test2 "x+x+x"  "3 x"
        test2 "x+2x+y+3x" "y+6x"
        test2 "f[x]+2f[x]+g[x]+f[x]" "4f[x]+g[x]"
      it "times" $ do
        test2 "x x x" "x^3"
        test2 "x^2 y x" "y x^3"
        test2 "x^2+x^3+x^6+x^2" "x^3+x^6+2x^2"
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
      -- it "merge same head" $ do
      --   test1 "x*x*2" $ List [Atom "Times", integer 2, List [Atom "Power", Atom "x",integer 2]]

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

  context "Eval.Primi.Control.Branch" $ do
    context "If" $ do
      it "If expression" $ do
        test1 "If[True,1,2]" (readVal "1")
        test1 "If[False,1,2]" (readVal "2")
        test1 "If[1,2,3,4]" (readVal "4")

  context "Eval.Primi.Primi" $ do
    context "CompoundExpression" $ do
      it "return last value" $ do
        test1 "a;b;c;1" one
        test2 "a;b;c" "c"

  context "Eval.Lambda" $ do
    context "slot type" $ do
      it "slot slotsequence" $ do
        test2 "({#1,Plus[##2]}&) @@@(Range/@Range[2,3])" "{{1,2},{1,5}}"
        test2 "(#[[1]]+#[[2]]&) /@{{1,2},{3,4,5},{6,7}}" "{3,7,13}"
        test2 "((#+##&) @@#&) /@{{1,2},{2,2,2},{3,4}}" "{4,8,10}"
    context "explicit Function" $ do
      it "Function" $ do
        test2 "Function[{x,y},x y][2,3]" "6"
        test2 "Function[x,2 x][5]" "10"
        test2 "(Function@@{{x},x==2})[2]" "True"

  context "Eval.Primitive.Primi.Replace.Replace" $ do
    context "Replace" $ do
      it "replace at different level" $ do
        test2 "Replace[x,x -> 1]" "1"
        test2 "Replace[{x,y},x -> 1]" "{x,y}"
        test2 "Replace[{x,y},{_,_} -> 1]" "1"
        test2 "Replace[{x,y,z},x -> 1,1]" "{1,y,z}"
        test2 "Replace[{{x},x,{{x}}},x -> 1,2]" "{{1},1,{{x}}}"
        test2 "Replace[{x,{x}},x -> 1,{2}]" "{x,{1}}"
        test2 "Replace[{x,x[x]},x -> 1,2]" "{1,x[1]}"

    context "ReplaceAll" $ do
      it "ReplaceAll" $ do
        test2 "{x,y,z}/.x -> 1" "{1,y,z}"
        test2 "{x[x],y}/.x -> 1" "{1[1],y}"
        test2 "{{x,y}}/.x:>Sequence[2,3]" "{{2,3,y}}"
        test2 "{{x,y},y}/.{_,_} -> {1,1}" "{1,1}"

    context "ReplaceRepeated" $ do
      it "ReplaceRepeated" $ do
        test2 "f[g[x],y]//.{f[x_,y_]:>k[g[x],g[y]],g[g[x_]]:>g[x]}" "k[g[x],g[y]]"
        test2 "x//.x -> 1" "1"

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
          "fib[1]=1;fib[2]=1", "fib[40]"]
        (integer 102334155)

    context "Nest,NestList" $ do
      context "Nest" $ do
        it "nest a fucntion" $ do
          test ["f[x_]=x^2","Nest[f,2,3]"] (integer 256)
          test2 "Nest[f,x,4]" "f[f[f[f[x]]]]"
      context "NestList" $ do
        it "Nestlist" $ do
          test2 "NestList[f,x,3]" "{x,f[x],f[f[x]],f[f[f[x]]]}"
          test2 "NestList[f,x,10][[{2,4},1]]" "{x,f[f[x]]}"

    it "pattern matching" $ do
      test
        ["a[0]=1","a[0.0]=1","a[0]","a[0.0]"] (integer 1)
    it "pattern test" $ do
      test
        ["f[a_]=a","f[a_?(#>10&)]=10", "f[1100]"] (integer 10)
      test
        ["12/.x_?(#>111&)->1"] (integer 12)
      test3
        ["f[a_?(False&)]=100", "f[1000]"]  "f[1000]"
      test
        ["fib[n_]:=fib[n]=fib[n-1]+fib[n-2]",
          "fib[n_?(#==1&)]=1", "fib[n_?(#==2&)]=1", "fib[40]"]
        (integer 102334155)
      test
        ["zero[_]=False","zero[0]=True",
          "f[_]=19","f[_?zero]=100", "f[0]"]  (integer 100)

      test
        ["f[x_, x_] = 100", "f[1,1]"] (integer 100)
      test3
        ["f[x_, x_]= 100", "f[1,2]"] "f[1,2]"
      test3
        ["{1,2,3}/.{x_,x_,y_} -> 2"] "{1,2,3}"
      test
        ["{1,1,2}/.{x_,x_,y_} -> 2"] (integer 2)
      test
        ["{1,1,2,2,3}/.{x_,x_,y_,y_,z_} -> 2"] (integer 2)

      test3
        ["{{1,1},{0,0},{0,2}}/.{x_,y_}/;x+y==2 -> a"] "{a,{0,0},a}"
      test3
        ["{{1,1},{0,0},{0,2}}/.{x_,x_}/;x+x==2 -> a"] "{a,{0,0},{0,2}}"
      test3
        ["Condition[Condition[f[x_],x>1],x<2]=sdf","f[3/2]"] "sdf"
      test3
        ["Condition[Condition[f[x_],x>1],x<2]=sdf","f[0]"] "f[0]"
      test3
        ["Condition[Condition[f[x_],x>1],x<2]=sdf","f[2]"] "f[2]"
      test3
        ["q[i_,j_]:=q[i,j]=q[i-1,j]+q[i,j-1];q[i_,j_]/;i<0||j<0=0;q[0,0]=1"
          ,"q[5,5]"] "252"
    it "sequence test" $ do
      -- Tests for sequence !!!
      test3 ["{1,2,3}/.{x__,y_} -> y"] "3"
      test3 ["f[x_,y__,z_]=Plus[x,y,z]", "f[1,2,3,4,5]"] "15"
      test3 ["f[a_*b__]:=f[a]+Sequence@@(Map[f,{b}])", "f[x y z k l]"]
        "f[k]+f[l]+f[x]+f[y]+f[z]"
      test3 ["f[x__] := Length[{x}]", "{f[x, y, z], f[]}"]
        "{3,f[]}"
      test3 ["f[x___] := p[x, x]","{f[], f[1], f[1, a]}"]
        "{p[],p[1,1],p[1,a,1,a]}"
      test3 ["f[x___]:=p[x,Plus[x]]", "{f[1], f[1,2],f[1,2,x],f[1,2,3]}"]
        "{p[1,1],p[1,2,3],p[1,2,x,3+x],p[1,2,3,6]}"
      test3 ["f[x_,y___]:=Plus[y]^x", "{f[1,2,3], f[23,5,2], f[23,af,23,l],f[]}"]
        "{5,27368747340080916343,(23+af+l)^23,f[]}"
      test3 ["f[a, b, c] /. f[x__] -> p[x, x, x]"] "p[a,b,c,a,b,c,a,b,c]"
      test3 ["h[a___, x_, b___, x_, c___] := hh[x] h[a, b, c]","h[2, 3, 2, 4, 5, 3]"] "h[4,5] hh[2] hh[3]"
      test3
        ["patt={x___,y_,z_,e___}/;y>z -> {x,z,y,e} ","{12,1,4,2,6,8,3,1,3456,12,6,1,43,1}//.patt"]
        "{1,1,1,1,2,3,4,6,6,8,12,12,43,3456}"

    it "alternative test" $ do
      test3 ["{a, b, c, d, a, b, b, b} /. a | b -> x"] "{x,x,c,d,x,x,x,x}"

    it "head test" $ do
      test3 ["f[x_g]=g","{f[g[2]], f[2]}"] "{g,f[2]}"
      test3 ["f[x_Integer]=int", "{f[2], f[2.0],f[g]}"] "{int,f[2.0],f[g]}"
      test3 ["f[x_Symbol]=sym", "{f[2], f[a], a=1;f[a]}"] "{f[2],sym,f[1]}"

    it "sequence head test" $ do
      test3 ["f[x__Integer]=2", "{f[2,3],f[a,2],f[2,a],f[2]}"] "{2,f[a,2],f[2,a],2}"
      test3 ["f[x__Real] := Plus[x]/Length[{x}]", "{f[1.0,4.0],f[2,2],f[1.0,a]}"] "{2.5,f[2,2],f[1.0,a]}"

    it "sequence pattern test" $ do
      test3 ["f[___,y__?(#>2&)]={y}", "{f[2,3],f[1,1,1,2],f[112,1,1,3,4]}"] "{{3},f[1,1,1,2],{3,4}}"



    it "symbolic manipulation" $ do
      test3
        ["rules = {Log[x_ y_] :> Log[x] + Log[y], Log[x_^k_] :> k Log[x]}",
        "Log[a (b c^d)^e] //. rules"] "Log[a]+e (Log[b]+d Log[c])"
      test3
        ["{f[f[x]], f[x], g[f[x]], f[g[f[x]]]} //. f[x_] -> x"] "{x,x,g[x],g[x]}"

    it "derivative" $ do
      test3
        ["D[a_,x_]=0","D[x_,x_]:=1", "D[a_+b__,x_]:=D[a,x]+D[Plus[b],x]",
          "D[a_ b__,x_]:=D[a,x] b+a D[Times[b],x]",
          "D[a_^(b_), x_]:= a^b(D[b,x] Log[a]+D[a,x]/a b)",
          "D[Log[a_], x_]:= D[a, x]/a",
          "D[Sin[a_], x_]:= D[a,x] Cos[a]",
          "D[Cos[a_], x_]:=-D[a,x] Sin[a]","D[x/Sin[x]/Cos[x]^2,x]"]
        "Plus[Times[x,Plus[Times[-1,Power[Cos[x],-1],Power[Sin[x],-2]],Times[2,Power[Cos[x],-3]]]],Times[Power[Cos[x],-2],Power[Sin[x],-1]]]"


main = hspec spec
