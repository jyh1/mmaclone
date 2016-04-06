module Eval.Primitive.Primi.Control.Branch(ifl) where
import Data.DataType
import Eval.Primitive.PrimiType

import Data.Maybe


ifl = manynop "If" 3 4 ifl'

if3Args :: [LispVal] -> Maybe LispVal
if3Args [predict, r1,r2]
  | isBool predict = Just $ if trueQ predict then r1 else r2
  | otherwise = Nothing

ifl' :: [LispVal] -> Result
ifl' args
  | length args == 3 = return (if3Args args)
  | otherwise = hasValue $ fromMaybe (last args) (if3Args (init args))
