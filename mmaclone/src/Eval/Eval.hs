{-#LANGUAGE ExistentialQuantification#-}
module Eval.Eval
    (
    eval,
    evalWithRecord,
    eval'
    ) where

import Data.DataType
import Data.Environment.Environment
import Data.Number.Number
import Eval.Primitive.Primitives
import Eval.Primitive.PrimiType hiding(eval)
import Eval.Environment
import Eval.EvalHead
import Data.Attribute


import Control.Monad
import Data.Ratio
import Data.Maybe(fromMaybe)
import Data.List(sort)
import Control.Monad.Except
import qualified Data.Map.Strict as M
import Control.Monad.Trans.State

evalWithRecord :: Env -> Int -> LispVal -> IOThrowsError LispVal
evalWithRecord env nn val = do
  let n = integer nn
      evaluate = eval env
  evaluate (List [Atom "Set",atomLine, n])
  evaluate (List [Atom "SetDelayed", List [Atom "In", n], val])
  evaled <- eval env val
  evaluate (List [Atom "Set", List [Atom "Out", n], evaled])

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val = do
  x1 <- eval' env val
  if x1 == val then return x1 else eval env x1

eval' :: Env -> LispVal -> IOThrowsError LispVal
eval' env (List (v:vs)) = do
  let evalE = eval env
  headE <- evalE v
  args <- attributeEvaluateArgs evalE headE vs
  let expr = List (headE : args)
  evaled <- evalHead headE env expr
  attTransform evaled

eval' env val@(Atom _) = evalWithEnv env val

eval' _ n@(Number (Rational r))
  | denominator r == 1 = return (Number $ Integer $ numerator r)
  | otherwise = return n

eval' _ x = return x

-- eval head --------------------------------------
evalPrimitiveHead :: LispVal -> Env -> Maybe LispFun
evalPrimitiveHead (Atom name) env = do
  primi <- M.lookup name primitives
  let evalFun (List val) = do
        context <- readCont env
        let primiEnv = PrimiEnv (eval env) context val False
        evalStateT primi primiEnv
  return evalFun

evalHead :: LispVal -> Env -> LispFun
evalHead h@(Atom _) env =
  fromMaybe (evalWithEnv env) (evalPrimitiveHead h env)
evalHead (List (Atom "Function":rest)) _ =
  evalLambda

evalHead other _ = return
-- ------------------------------------------------


-- attribute relating functions
-- | evaluate arguments under the attributes specification of Head
attributeEvaluateArgs :: (LispVal -> IOThrowsError LispVal) ->
  LispVal -> [LispVal] -> IOThrowsError [LispVal]
attributeEvaluateArgs evalE h rests = do
  let att = getAttributes h attributes
  evaled <- attEvalHold evalE att rests
  return $ allAttr att h evaled

-- | handle HoldAll HoldFirst HoldRest
attEvalHold:: (LispVal -> IOThrowsError LispVal) ->
  [Attribute] -> [LispVal] -> IOThrowsError [LispVal]
attEvalHold evalE atts vals
  | elem HoldAll atts = return vals
  | elem HoldFirst atts = do
      rest <- mapM evalE (tail vals)
      return (head vals : rest)
  | elem HoldRest atts = do
      first <- evalE (head vals)
      return (first : tail vals)
  | otherwise = mapM evalE vals


attTransform :: LispVal -> IOThrowsError LispVal
attTransform val = return (attributeTransform attributes val)
