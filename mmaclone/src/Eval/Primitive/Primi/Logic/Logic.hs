module Eval.Primitive.Primi.Logic.Logic(andl,orl,notl) where
import Data.DataType
import Data.Number.Number
import Eval.Primitive.PrimiType

andl = binop "And" andl'
orl = binop "Or" orl'
notl = sinop "Not" notl'

logic :: (Bool -> Bool -> Bool) -> BinaryFun
logic f a b
  |isBool a && isBool b = hasValue $ toBool (unBool a `f` unBool b)
  | otherwise = noChange

andl', orl' :: BinaryFun
andl' =logic (&&)
orl' =logic (||)

notl' :: SingleFun
notl' (Atom "True") = hasValue false
notl' (Atom "False") = hasValue true
notl' _ = noChange

true = Atom "True"
false = Atom "False"
