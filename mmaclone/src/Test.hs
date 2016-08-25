module Test where
import Data.Number.Number hiding(plus,times,one)
import Data.DataType hiding (list,addHead)
import Parser.Trans
import Eval.Eval

import Data.Environment.Environment
import Control.Monad.Except
import Control.Monad.Trans.State


import System.IO.Unsafe

addHead a b = List (Atom a : b)

list = addHead "List"

plus = addHead "Plus"

times = addHead "Times"

comp = addHead "CompoundExpression"

part = addHead "Part"

map' = addHead "Map"
mapAll = addHead "MapAll"
apply = addHead "Apply"
apply1 [l1,l2] = apply [l1,l2,list [one]]

replace = addHead "ReplaceAll"
replaceR = addHead "ReplaceRepeated"
rule = addHead "Rule"
ruleD = addHead "RuleDelayed"

set = addHead "Set"
setD = addHead "SetDelayed"

unset = addHead "Unset" . return

fun = addHead "Function"
slot = addHead "Slot" . return
s1 = slot one
s2 = slot two
ss1 = addHead "SlotSequence" [one]

cond = addHead "Condition"

deriv n l = List [List [Atom "Derivative", integer n],l]

fact = addHead "Factorial" . return
fact2 = addHead "Factorial2" . return

patt = addHead "Pattern"
pattT = addHead "PatternTest"
blk = List [Atom "Blank"]

andE = addHead "And"
orE = addHead "Or"
notE = addHead "Not"
ineq = addHead "Inequality"

dot = addHead "Dot"

alter = addHead "Alternatives"

equal = Atom "Equal"
less = Atom "Less"
lessEq = Atom "LessEqual"
great = Atom "Greater"
greatEq = Atom "GreaterEqual"
unEq = Atom "Unequal"

one = integer 1
two = integer 2
three = integer 3

pe = Atom "P"

rational = Number . Rational

readVal = extractValue . readExpr

testEvalWith :: (LispVal -> Primi) -> String -> LispVal
testEvalWith eval expr =
  let val = readVal expr
      evaled = unsafePerformIO.runExceptT $ evalStateT (eval val) initialState in
    extractValue evaled

testEvalOnce = testEvalWith eval'
runEval = testEvalWith eval
