module Eval.Primitive.Primi.Logic.Not(notl) where
import Data.DataType
import Data.Number.Number
import Eval.Primitive.PrimiType
-- | Function Not primitive
notl = sinop "Not" notl'

notl' :: SingleFun
notl' (Atom "True") = hasValue false
notl' (Atom "False") = hasValue true
notl' _ = noChange
