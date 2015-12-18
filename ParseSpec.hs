module ParseSpec where

import Parse
import DataType

import Test.Hspec
import Test.QuickCheck
import Control.Exception(evaluate)

spec :: Spec
spec  = do
  describe "readExpr parse a string to LispVal" $ do
    context "when provided atom" $ do
      it "read an atom expression" $ do
        readExpr "abc" == Atom "abc"

    context "when provided integer" $ do
      it "read an integer" $ do
        readExpr "3435" == Number 3435

    context "when provided float" $ do
      it "read with decimal point" $ do
        readExpr "23.6" == Float 23.6

      it "read float with e and decimal point" $ do
        readExpr "23.6e5" == Float 23.6e5

      it "read float without decimal point" $ do
        readExpr "24e5" == Float 24e5

      it "read float withou decimal point capital E" $ do
        readExpr "24E5" == Float 24E5

    context "when provided string" $ do
      it "read a common string" $ do
        readExpr "\"a string\"" == String "a string"

      it "with standard" $ do
        readExpr "\"\\n\\t\\\"\\\\\"" == String "\n\t\"\\"

    context "when provided char" $ do
      it "read a char" $ do
        readExpr "\'a\'" == Char 'a'

    context "when provided bool" $ do
      it "read true" $ do
        readExpr "#t" == Bool True

      it "read false" $ do
        readExpr "#f" == Bool False

    context "read a list of values" $ do
      it "read a list of different value" $ do
        readExpr "(a   1 2.0 \"ss\" )" == List [Atom "a", Number 1, Float 2.0, String "ss"]

      it "read abitray nested list" $ do
        readExpr "(((a) b))" == List [List [List [Atom "a"], Atom "b"]]

    context "read a dotted list" $ do
      it "read DottedList" $ do
        readExpr "(2 #t . #f)" == DottedList [Number 2, Bool True] (Bool False)

      it "read nested dotted list" $ do
        readExpr "(3 . ((2) . 1))" == DottedList [Number 3] (DottedList [List [Number 2]] (Number 1))
