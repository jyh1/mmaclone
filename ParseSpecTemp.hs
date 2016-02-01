module NewParseSpec where

import NewParse
import DataType
import Number

import Test.Hspec
import Test.QuickCheck
import Control.Exception(evaluate)

testRead = extractValue . readExpr

test a b = testRead a `shouldBe` b

testList a b = test a (List b)

true = Bool True
false = Bool False

blank = Atom "Blank"

makeBlank "" "" = List [blank]
makeBlank "" b = List [blank, Atom b]
makeBlank a "" = List [Atom "Pattern",Atom a ,List [blank]]
makeBlank a b = List [Atom "Pattern", Atom a, List [blank, Atom b]]

spec :: Spec
spec  = do
  describe "testRead parse a string to LispVal" $ do
    context "when provided atom" $ do
      it "read an atom expression" $ do
        test "abc" $ Atom "abc"
        -- test "True" true
        -- test "False" false
        test "111" (integer 111)
        test "23.6" (double 23.6)
        test "23.6e5" (double 23.6e5)
        test "-32" (integer (-32))
    context "parse expression with arguments" $ do
      it "prefix form" $ do
        testList "P[]" [Atom "P"]
        testList "P []" [Atom "P"]
        testList "P [a, b]" [Atom "P", Atom "a", Atom "b"]
        testList "P [B [a], 23]" [Atom "P", List [Atom "B", Atom "a"],integer 23]
    context "parse string" $ do
      it "read a common string" $ do
        test "\"a string\"" $ String "a string"
      it "with standard" $ do
        test "\"\\n\\t\\\"\\\\\"" $ String "\n\t\"\\"
    context "parse blank pattern" $ do
      it "no binding name" $ do
        test "_" (makeBlank "" "")
        testList "__Ki" [Atom "BlankSequence", Atom "Ki"]
        testList "K[_, _]" [Atom "K", makeBlank "" "", makeBlank "" ""]
      it "with binding name" $ do
        test "AA_K" (makeBlank "AA" "K")
        testList "Q[_, K_,C]" [Atom "Q", makeBlank "" "", makeBlank "K" "", Atom "C"]

main = hspec spec
