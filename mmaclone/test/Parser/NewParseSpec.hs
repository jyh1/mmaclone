module Parser.NewParseSpec where

import Parser.NewParse
import Data.Number.Number

import Text.Parsec
import Test.Hspec
import Test.QuickCheck hiding (Args)
import Control.Exception(evaluate)

extractValue (Right a) = a

testRead = extractValue . parseExpr


test a b = testRead a `shouldBe` b

testApply a b c = test a $ Apply (Var b) (Args c)

integer = Num . Integer
double = Num . Double

preS = "F[a,b,c]"
pre = Apply (Var "F") (Args [Var "a", Var "b", Var "c"])

pe = Var "P"

spec :: Spec
spec  = do
  describe "testRead parse a string to LispVal" $ do
    context "when provided atom" $ do
      it "read an atom expression" $ do
        test "abc" $ Var "abc"
        -- test "True" true
        -- test "False" false
        test "111" (integer 111)
        test "23.6" (double 23.6)
        test "23.6e5" (double 23.6e5)
        test "-32" $ Negate (integer 32)
    context "parse expression with arguments" $ do
      it "prefix form" $ do
        testApply "P[]" "P" []
        testApply "P []" "P" []
        testApply "P [a, b]" "P" [Var "a", Var "b"]
        testApply "P [B [a], 23]" "P" [Apply (Var "B") (Args [Var "a"]) ,integer 23]
    context "part expression" $ do
      it "part expression" $ do
        test "P[[a]]" (Part pe (PartArgs [Var "a"]))
        test "P[[P[x],a]]" (Part pe (PartArgs [Apply pe (Args [Var "x"]),Var "a"]))
    context "operator" $ do
      it "@ function apply" $ do
        test "P@c" (Apply pe (Args [Var "c"]))
        test "P@P@P" (Apply pe (Args $ [Apply pe (Args [pe])]))
      it "// apply" $ do
        test "P//p" (Apply (Var "p") (Args [pe]))
        test "P//p//a" (Apply (Var "a") (Args [Apply (Var "p") (Args [pe])]))
      it "/@ map" $ do
        test "P/@P" (Map pe pe)
        test "P/@P@c" (Map pe (Apply pe (Args [Var "c"])))
      it "@@ apply" $ do
        test "P@@P" (Apply1 pe pe)
      it "derivative" $ do
        test "P''[x]" (Apply (Derivative 2 pe) $ Args [Var "x"])
        -- test "P'"
      it "dot" $ do
        test "P . P" (Dot pe pe)

      it "not factorial" $ do
        test "!a" (Not (Var "a"))
        test "a!" (Fact (Var "a"))

      it "replace /." $ do
        test "P/.P->P" (Replace pe (Rule pe pe))
        test "P/.{P->P, P->P}" (Replace pe (Lis [Rule pe pe,Rule pe pe]))

      it "& function" $ do
        test "P&" (Function pe)
        test "(P&) @P" (Apply (Function pe) (Args [pe]))
    context "parse string" $ do
      it "read a common string" $ do
        test "\"a string\"" $ Str "a string"
      it "with standard" $ do
        test "\"\\n\\t\\\"\\\\\"" $ Str "\n\t\"\\"
    context "parse a literal char" $ do
      it "read a char" $ do
        test "\'c\'" $ Chr 'c'
        test "\'\\n\'" $ Chr '\n'
    context "parse blank pattern" $ do
      it "blank pattern" $ do
        test "_" Blk
        test "_P" (BlkE pe)
        test "P_P" (Pattern pe (BlkE pe))
        test "__" BlkSeq
        test "__P" (BlkSeqE pe)
        test "P__P" (Pattern pe (BlkSeqE pe))
        test "___" NullSeq
        test "___P" (NullSeqE pe)
        test "P___P" (Pattern pe (NullSeqE pe))
        test "_[P]" $ Apply Blk (Args [pe])
    context "# slot" $ do
      it "slot" $ do
        test "#" (Slot 1)
        test "#2" (Slot 2)
        test "1+#" (Add (integer 1) (Slot 1))
      it "slot sequence" $ do
        test "##" (SlotSeq 1)
        test "##6" (SlotSeq 6)
        test "P@@##" (Apply1 pe (SlotSeq 1))
    context "% Out" $ do
      it "% Out" $ do
        test "%" (Out (-1))
        test "%%" (Out (-2))
        test "%4" (Out 4)
        test "P[%]" (Apply pe (Args [Out (-1)]))
        test "#%" (Mul (Slot 1) (Out (-1)))

    context "compound expression" $ do
      it "compound" $ do
        test "P;#" (Compound pe (Slot 1))
        -- test "P;1;2" (Compound [pe,integer 1,integer 2, None])

    context "conditional expression" $ do
      it "condtion" $ do
        test "P_ -> 1/;P" (Rule (Pattern pe Blk) (Cond (integer 1) pe))
        test "P=1/;P" (Set pe (Cond (integer 1) pe))

    context "pattern alternative" $ do
      it ": pattern" $ do
        test "P:_" (Pattern pe Blk)
        test "P:(1|2)" (Pattern pe (Alter (integer 1) (integer 2)))

main = hspec spec
