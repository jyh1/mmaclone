module Parser.TransSpec where

import Parser.Trans
import Parser.NewParse
import Data.Number.Number hiding(plus,times,one,less,lessEqual,greater,greaterEqual,equal)
import Data.DataType hiding (list)
import Test

import Test.Hspec
import Test.QuickCheck hiding (Args)
import Control.Exception(evaluate)

-- extractValue (Right a) = a

testRead = extractValue . transform . parseExpr

test a b = testRead a `shouldBe` b


spec :: Spec
spec  = do
  describe "transform an Expr to LispVal" $ do
    context "number" $ do
      it "transform a number" $ do
        test "123" (integer 123)
        test "12.3" (double 12.3)
    context "list" $ do
      it "transform a list" $ do
        test "{1}" (list [integer 1])
        test "{1,2}" (list [integer 1, integer 2])
    context "plus" $ do
      it "flatten plus" $ do
        test "1+2" (plus [one, two])
        test "1+2+3" (plus [one,two,three])
        test "P+2+3+1" (plus [pe, two, three,one])
      it "minus" $ do
        test "1-2" (plus [one, integer (-2)])
        test "1+2-3" (plus [one, two, integer (-3)])
        test "1+2-P" (plus [one, two, negateE pe])

    context "times" $ do
      it "times" $ do
        test "1 2" (times [one, two])
        test "1 P 3" (times [one, pe, three])
        test "2 * 3" (times [two, three])
      it "divide" $ do
        test "1 /2" (times [one, inverseE two])
        test "1 2/3" (times [one, two, inverseE three])
        test "1/P *2" (times [one,inverseE pe, two])
    context "logical expression" $ do
      it "and logic" $ do
        test "1&&2" (andE [one, two])
        test "P&&1" (andE [pe, one])
        test "P&&1&&2" (andE [pe, one,two])
      it "or" $ do
        test "1||2||3" (orE [one,two,three])
        test "1&&P||2" (orE [andE [one,pe], two])
      it "not" $ do
        test "!P&&P" (andE [notE [pe], pe])

    context "compound expression" $ do
      it "compound" $ do
        test "1;2;3" (comp [one,two,three])
        -- test "1;2;P;" (comp [one,two,pe,atomNull])

    context "Inequality" $ do
      it "test equal" $ do
        test "1==2" (ineq [one,equal,two])
        test "1==2==3" (ineq [one, equal,two,equal,three])
      it "comb" $ do
        test "1==2>=3" (ineq [one,equal,two,greatEq,three])
        test "1<=2+P!=3" (ineq [one, lessEq,plus [two,pe],unEq ,three])
        test "1<2>3" (ineq [one,less,two,great,three])
        test "1<=1+2!=3" (ineq [one,lessEq,plus [one,two],unEq,three])

    context "function apply" $ do
      it "nest apply" $ do
        test "P[P[1]]" $ List [pe,List [pe, one]]
        test "P[1,2]" $ List [pe, one,two]
      it "curry" $ do
        test "P[1][2]" $ List [List [pe,one], two]
      it "operator form" $ do
        test "P@1@2//3" $ List [three,List [pe,List [one,two]]]
        test "P@1+2//3" $ List [three,plus [List [pe,one],two]]

    context "part" $ do
      it "part expression" $ do
        test "P[[1,2]]" $ part [pe,one,two]
        test "P[1][[2]]" $ part [List [pe,one],two]
        test "P[2[[1]]]" $ List [pe, part [two, one]]
    context "Atom" $ do
      it "atom name" $ do
        test "P" pe

    context "Factorial" $ do
      it "factorial !" $ do
        test "2!" (fact two)
        test "1+2!" (plus [one,fact two])
        test "2! != 3!" (ineq [fact two,unEq,fact three])
        test "2!! != P!" (ineq [fact2 two,unEq,fact pe])

    context "Map,MapAll,Apply,Apply1" $ do
      it "Map" $ do
        test "P/@{1,2}" (map' [pe,list [one,two]])
        test "P/@P/@1" (map' [pe,map' [pe,one]])
        test "P/@1@2" (map' [pe, List [one,two]])

      it "MapAll" $ do
        test "P//@1" (mapAll [pe,one])
        test "P//@1/@2" (mapAll [pe,map' [one,two]])

      it "apply" $ do
        test "P@@1/@2@3" (apply [pe,map' [one,List [two,three]]])
        test "P@@@2@@1" (apply1 [pe,apply [two,one]])

    context "derivative" $ do
      it "nth derivative" $ do
        test "P'" (deriv 1 pe)
        test "P''''" (deriv 4 pe)
      it "apply to args" $ do
        test "P''[1]" (List [deriv 2 pe,one])
        test "P'[1][2]" (List [List [deriv 1 pe,one],two])

    context "replace rule" $ do
      it "replace rule operators" $ do
        test "P/.1 -> 2" (replace [pe,rule [one,two]])
        test "P /. (1 //. 2  -> 3) -> P" (replace [pe,rule [replaceR [one,rule [two,three]],pe]])
        test "P//.2:>3" (replaceR [pe,ruleD [two,three]])
      it "condition replace" $ do
        test "1/.P_ -> 2/;P>=1" (replace [one,rule [patt [pe,blk],cond [two,ineq [pe,greatEq,one]]]])
      it "alternative replace" $ do
        test "1/.1|2 -> 3" (replace [one,rule [alter [one,two],three]])

    context "Set SetDelayed" $ do
      it "set" $ do
        test "P=1" (set [pe,one])
        test "P:=1=2" (setD [pe,set [one,two]])
        test "P=1+2" (set [pe,plus [one,two]])

    context "Unset" $ do
      it "unset a var" $ do
        test "P=." (unset pe)

    context "Dot" $ do
      it "Dot" $ do
        test "P.{1,2}" (dot [pe,list [one,two]])
        test "{1,2}.{2,3}" (dot [list [one,two],list [two,three]])

    context "Pattern special form" $ do
      it "special form" $ do
        test "P_" (patt [pe, blk])
        test "P_?(1+2)" (pattT [patt [pe,blk], plus [one,two]])

    context "lambda function" $ do
      it "& operator" $ do
        test "(1+#)&" (fun [plus [one,s1]])
        test "(1+#&)[2]" (List [fun [plus [one,s1]],two])
        test "(1 ##&)[2]" (List [fun [times [one,ss1]],two])

    context "alternative" $ do
      it "| operator" $ do
        test "1|2|3" (alter [one,two,three])

main = hspec spec
