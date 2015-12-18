module ParseSpec where

import Parse
import DataType

import Test.Hspec
import Test.QuickCheck
import Control.Exception(evaluate)

testRead = extractValue . readExpr

spec :: Spec
spec  = do
  describe "testRead parse a string to LispVal" $ do
    context "when provided atom" $ do
      it "read an atom expression" $ do
        testRead "abc" == Atom "abc"

    context "when provided integer" $ do
      it "read an integer" $ do
        testRead "3435" == Number 3435

    context "when provided float" $ do
      it "read with decimal point" $ do
        testRead "23.6" == Float 23.6

      it "read float with e and decimal point" $ do
        testRead "23.6e5" == Float 23.6e5

      it "read float without decimal point" $ do
        testRead "24e5" == Float 24e5

      it "read float withou decimal point capital E" $ do
        testRead "24E5" == Float 24E5

    context "when provided string" $ do
      it "read a common string" $ do
        testRead "\"a string\"" == String "a string"

      it "with standard" $ do
        testRead "\"\\n\\t\\\"\\\\\"" == String "\n\t\"\\"

    context "when provided char" $ do
      it "read a char" $ do
        testRead "\'a\'" == Char 'a'

    context "when provided bool" $ do
      it "read true" $ do
        testRead "#t" == Bool True

      it "read false" $ do
        testRead "#f" == Bool False

    context "read a list of values" $ do
      it "read a list of different value" $ do
        testRead "(a   1 2.0 \"ss\" )" == List [Atom "a", Number 1, Float 2.0, String "ss"]

      it "read abitray nested list" $ do
        testRead "(((a) b))" == List [List [List [Atom "a"], Atom "b"]]

    context "read a dotted list" $ do
      it "read DottedList" $ do
        testRead "(2 #t . #f)" == DottedList [Number 2, Bool True] (Bool False)

      it "read nested dotted list" $ do
        testRead "(3 . ((2) . 1))" == DottedList [Number 3] (DottedList [List [Number 2]] (Number 1))
