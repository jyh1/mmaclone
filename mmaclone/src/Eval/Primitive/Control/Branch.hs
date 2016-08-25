module Eval.Primitive.Control.Branch(ifl) where
import Data.DataType
import Eval.Primitive.PrimiFunc
import Data.Environment.EnvironmentType

import Data.Maybe

ifl :: Primi
ifl = do
  between 3 4
  usesArgumentMaybe ifl'

if3Args :: [LispVal] -> Maybe LispVal
if3Args [predict, r1,r2]
  | isBool predict = Just $ if trueQ predict then r1 else r2
  | otherwise = Nothing

ifl' :: [LispVal] -> Maybe LispVal
ifl' args
  | length args == 3 = if3Args args
  | otherwise = Just $ fromMaybe (last args) (if3Args (init args))
