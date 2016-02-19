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
import Eval.Primitive.PrimiType
import Eval.Environment
import Eval.Lambda
import Data.Attribute

import Control.Monad
import Data.Ratio
import Data.Maybe(fromMaybe)
import Data.List(sort)
import Control.Monad.Except
import qualified Data.Map.Strict as M
evalWithRecord :: Env -> Int -> LispVal -> IOThrowsError LispVal
evalWithRecord env nn val = do
  let n = integer $ fromIntegral nn
  evaled <- eval env val
  eval env (List [Atom "SetDelayed", List [Atom "In", n], val])
  eval env (List [Atom "Set", List [Atom "Out", n], evaled])

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
  fun <- M.lookup name primitives
  let evalFun val@(List (_:args)) =
        liftM (fromMaybe val) $ fun env args
  return evalFun

evalHead :: LispVal -> Env -> LispFun
evalHead h@(Atom _) env =
  fromMaybe (evalWithEnv env) (evalPrimitiveHead h env)
evalHead (List (Atom "Function":rest)) _ =
  evalLambda

evalHead other _ = return
-- ------------------------------------------------


-- attribute relating functions
attributeEvaluateArgs :: (LispVal -> IOThrowsError LispVal) ->
  LispVal -> [LispVal] -> IOThrowsError [LispVal]
attributeEvaluateArgs evalE h rests = do
  let att = getAttributes h attributes
  evaled <- attEvalHold evalE att rests
  return $ allAttr att h evaled

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



-- require eval function
primitives = M.fromList $ primitives' ++ requireEval

requireEval :: [(String,IOPrimi)]
requireEval = [
                ("And",andl),
                ("Or",orl),
                ("Nest",nestl),
                ("NestList",nestListl)
              ]

-- logic ---------------------------------------------------
logic :: Eval ->
  LispVal ->
  [LispVal] -> IOThrowsError LispVal
logic _ triv [] = return triv
logic eval trivi (x:xs) =
  let rest = logic eval trivi xs
      check = (trivi ==) in
  do
    x' <- eval x
    if isBool x' then
      if check x' then
        rest
      else
        return x'
    else do
      restRes <- rest
      return $ case restRes of
        List res -> List (x':res)
        _ -> if check restRes then
           List [x'] else restRes

-- logicLift :: LispVal -> LispVal -> Maybe LispVal
logicLift _ val@(Atom _) = Just val
logicLift name val = Just (addHead (Atom name) val)

andl,orl :: IOPrimi
andl env ls =
  liftM (logicLift "And") $ logic (eval env) true ls

orl env ls =
  liftM (logicLift "Or") $ logic (eval env) false ls
-- ---------------------------------------------------

-- Nest-----------------------------------------------
nestl ,nestListl:: IOPrimi
nestl env = withnop 3 "Nest" (nestl' env)
nestListl env = withnop 3 "NestList" (nestListl' env)


nest,nestList :: Eval -> LispVal -> LispVal -> Int -> EvalResult
nest _ _ arg 0 = return arg
nest eval f arg n = do
  evaled <- eval (applyHead f arg)
  nest eval f evaled (n-1)

nestList' _ _ arg 0 = return [arg]
nestList' eval f arg n = do
  evaled <- eval (applyHead f arg)
  rest <- nestList' eval f evaled (n-1)
  return $ arg : rest
nestList eval f arg n = liftM list (nestList' eval f arg n)


nestErr = Default "Nest :: non-negative machine-sized number expected"
unpackNest3rd :: LispVal -> IOThrowsError Int
unpackNest3rd (Number (Integer n))
  | n >= 0 = return (fromIntegral n)
  | otherwise = throwError nestErr
unpackNest3rd _ = throwError nestErr

nestUnpack nest env [f,arg,n] = do
  n' <- unpackNest3rd n
  liftM Just $ nest (eval env) f arg n'

nestl' = nestUnpack nest

nestListl' = nestUnpack nestList

-- ---------------------------------------------------
