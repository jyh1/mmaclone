module Test where
import Data.Number.Number hiding(plus,times,one)
import Data.DataType hiding (list)
import Parser.Trans

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

replace = addHead "Replace"
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
great = Atom "Great"
greatEq = Atom "GreatEqual"
unEq = Atom "Unequal"

one = integer 1
two = integer 2
three = integer 3

pe = Atom "P"

rational = Number . Rational

true = toBool True
false = toBool False

readVal = extractValue . readExpr
