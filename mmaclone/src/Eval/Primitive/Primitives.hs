module Eval.Primitive.Primitives(primitives) where

import Eval.Primitive.PrimiFunc
import Data.Environment.EnvironmentType
import Eval.Primitive.Arithmatic.Arithmatic
import Eval.Primitive.List.List
import Eval.Primitive.Compare.Compare
import Eval.Primitive.Logic.Logic
import Eval.Primitive.Control.Branch
import Eval.Primitive.Function.Lambda
import Eval.Primitive.Replace.Replace
import Eval.Primitive.Nest.Nest
import Eval.Primitive.Set.Set
import Eval.Primitive.IO.Print
import Eval.Primitive.InOut.InOut


import qualified Data.Text as T
import qualified Data.Map.Strict as M

-- | Collections of all primitive function
primitives :: M.Map T.Text Primi
primitives = M.fromList
  [ ("CompoundExpression",compoundExpressionl)
  , ("Minus", minusl)
  , ("Divide", dividel)
  , ("Plus",plusl)
  , ("Times", timesl)
  , ("Power", powerl)
  , ("Log", logl)
  -- list mainpulation
  , ("car", carl)
  , ("cdr", cdrl)
  , ("cons", consl)
  , ("Length", lengthl)
  , ("Part", partl)
  , ("Map", mapl)
  , ("Apply",applyl)
  -- list construction
  , ("Range", rangel)

  -- comparation
  , ("Less", lessl)
  , ("LessEqual" , lessEquall)
  , ("Greater", greaterl)
  , ("GreaterEqual", greaterEquall)
  , ("Equal", equall)
  , ("Inequality",inequalityl)
  -- logic function
  , ("Not", notl)
  , ("And", andl)
  , ("Or", orl)
  -- branch
  , ("If",ifl)

  , ("Function", functionl)
  -- replace
  , ("Replace", replacel)
  , ("ReplaceAll",replaceAlll)
  , ("ReplaceRepeated", replaceRepeatedl)

  , ("Nest", nestl)
  , ("NestList", nestListl)

  , ("Set",setl)
  , ("SetDelayed", setDelayedl)

  , ("Print", printl)

  , ("In", inl)
  , ("Out", outl)

  , ("Condition", conditionl)
  , ("Pattern", patternl)
  ]


compoundExpressionl :: Primi
compoundExpressionl = do
  many1op
  fmap last getArgumentList

conditionl :: Primi
conditionl = do
  withnop 2
  noChange

patternl :: Primi
patternl = do
  withnop 2
  noChange
