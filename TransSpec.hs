module NewParseSpec where

import Trans
import NewParse
import Number hiding(plus,times)
import DataType

import Test.Hspec
import Test.QuickCheck hiding (Args)
import Control.Exception(evaluate)

-- extractValue (Right a) = a

testRead = extractValue . transform . parseExpr

test a b = testRead a `shouldBe` b

addHead a b = List (Atom a : b)

list = addHead "List"

plus = addHead "Plus"

times = addHead "Times"

andE = addHead "And"
orE = addHead "Or"
notE = addHead "Not"
ineq = addHead "Inequality"

equal = Atom "Equal"
less = Atom "Less"
lessEq = Atom "LessEqual"
great = Atom "Great"
greatEq = Atom "GreatEqual"
unEq = Atom "Unequal"

one = integer 1
two = integer 2
three = integer 3

pe = Atom "P"
-- testApply a b c = test a $ Apply (Var b) (Args c)

-- integer = Number . Integer
-- double = Number . Double

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

    context "Inequality" $ do
      it "test equal" $ do
        test "1==2" (ineq [one,equal,two])
        test "1==2==3" (ineq [one, equal,two,equal,three])
      it "comb" $ do
        test "1==2>=3" (ineq [one,equal,two,greatEq,three])
        test "1<=2+P!=3" (ineq [one, lessEq,plus [two,pe],unEq ,three])
        test "1<2>3" (ineq [one,less,two,great,three])
        test "1<=1+2!=3" (ineq [one,lessEq,plus [one,two],unEq,three])
    context "Atom" $ do
      it "atom name" $ do
        test "P" pe


main = hspec spec
