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
  let old = List (headE : args)
      getFName (Atom f) = Just f
      getFName _ = Nothing
  let fun = do
        name <- getFName headE
        M.lookup name primitives
  evaled <- case fun of
    Just f -> liftM (fromMaybe old) (f env args)
    Nothing -> evalWithEnv env old
  attTransform evaled

eval' env val@(Atom _) = evalWithEnv env val

eval' _ n@(Number (Rational r))
  | denominator r == 1 = return (Number $ Integer $ numerator r)
  | otherwise = return n

eval' _ x = return x

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

-- attEvalOrderless :: [Attribute] -> [LispVal] -> [LispVal]
-- attEvalOrderless att vals
--   | Orderless `elem` att = sort vals
--   | otherwise = vals
--
-- attEvalFlatten :: [Attribute] -> LispVal -> [LispVal] -> [LispVal]
-- attEvalFlatten att h vals
  -- | Flatten `elem` att = deleteSameHead vals h
--   | otherwise = vals
--
-- attEvalSeqHold :: [Attribute] -> [LispVal] -> [LispVal]
-- attEvalSeqHold att vals
--   | SequenceHold `elem` att = vals
--   | otherwise = deleteSameHead vals (Atom "Sequence")
--
-- allAttr :: [Attribute] -> LispVal-> [LispVal] -> [LispVal]
-- allAttr att h = attEvalOrderless att .attEvalFlatten att h .
--                   attEvalSeqHold att
-- ----------------------------

-- deleteSameHead :: [LispVal] -> LispVal -> [LispVal]
-- deleteSameHead [] _ = []
-- deleteSameHead (val@(List x):xs) h
--   | head x == h = tail x ++ deleteSameHead xs h
--   | otherwise = val : deleteSameHead xs h
-- deleteSameHead (x:xs) h = x : deleteSameHead xs h


-- require eval function
primitives = M.fromList $ primitives' ++ requireEval

requireEval :: [(String,IOPrimi)]
requireEval = [
                ("And",andl),
                ("Or",orl)
              ]

-- logic ---------------------------------------------------
logic :: (LispVal -> IOThrowsError LispVal) ->
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
