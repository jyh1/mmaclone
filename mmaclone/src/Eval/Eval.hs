{-#LANGUAGE ExistentialQuantification#-}
module Eval.Eval
    (
    eval,
    evalWithRecord,
    eval',
    initialState,
    Primi
    ) where

import Data.DataType
import Data.Environment.Environment
import Data.Number.Number
import Eval.Primitive.Primitives
import Eval.Primitive.PrimiType hiding(eval)
import Eval.EvalHead
import Data.Attribute


import Control.Monad
import Data.Ratio
import Data.Maybe(fromMaybe)
import Data.List(sort)
import Control.Monad.Except
import qualified Data.Map.Strict as M
import Control.Monad.Trans.State
import Control.Lens hiding (List, Context)

initialState = PrimiEnv eval nullContext [] False

evalWithRecord :: Int -> LispVal -> Primi
evalWithRecord nn val = do
  let n = integer nn
  eval (List [Atom "Set",atomLine, n])
  eval (List [Atom "SetDelayed", List [Atom "In", n], val])
  evaled <- eval val
  eval (List [Atom "Set", List [Atom "Out", n], evaled])

eval :: LispVal -> Primi
eval val = do
  x1 <- eval' val
  if x1 == val then return x1 else eval x1

eval' :: LispVal -> Primi
eval' (List (v:vs)) = do
  headE <- eval v
  arguments <- attributeEvaluateArgs headE vs
  args .= headE : arguments
  attTransform <$> evalHead headE

eval' val@(Atom _) = uses con (replaceContext val)

eval' n@(Number (Rational r))
  | denominator r == 1 = return (integer $ numerator r)
  | otherwise = return n

eval' x = return x

-- eval head --------------------------------------
evalPrimitiveHead :: LispVal -> Maybe Primi
evalPrimitiveHead (Atom name) =
  M.lookup name primitives
  -- let evalFun (List val) = do
  --       context <- readCont env
  --       let primiEnv = PrimiEnv eval context val False
  --       evalStateT primi primiEnv
  -- return evalFun

evalWithEnv :: Primi
evalWithEnv = do
  lhs <- noChange
  if validSet lhs
    then uses con (replaceContext lhs)
    else noChange

evalHead :: LispVal -> Primi
evalHead h@(Atom _) =
  fromMaybe evalWithEnv (evalPrimitiveHead h)
evalHead (List (Atom "Function":rest)) =
  evalLambda
evalHead _ = noChange
-- ------------------------------------------------


-- attribute relating functions
-- | evaluate arguments under the attributes specification of Head
attributeEvaluateArgs ::
  LispVal -> [LispVal] -> StateResult [LispVal]
attributeEvaluateArgs h rests = do
  let att = getAttributes h attributes
  evaled <- attEvalHold att rests
  return $ allAttr att h evaled

-- | handle HoldAll HoldFirst HoldRest
attEvalHold::
  [Attribute] -> [LispVal] -> StateResult [LispVal]
attEvalHold atts vals
  | elem HoldAll atts = return vals
  | elem HoldFirst atts = do
      rest <- mapM evaluate (tail vals)
      return (head vals : rest)
  | elem HoldRest atts = do
      first <- evaluate (head vals)
      return (first : tail vals)
  | otherwise = mapM evaluate vals


attTransform :: LispVal -> LispVal
attTransform val = attributeTransform attributes val
