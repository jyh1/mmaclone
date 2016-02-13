{-#LANGUAGE ExistentialQuantification#-}
module Eval.Eval
    (
    eval,
    evalWithRecord
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
  args <- attributeEvaluate evalE headE vs
  -- args <- mapM (eval env) vs'
  let old = List (headE : args)
      getFName (Atom f) = Just f
      getFName _ = Nothing
  let fun = do
        name <- getFName headE
        M.lookup name primitives
  case fun of
    Just f -> liftM (fromMaybe old) (f env args)
    Nothing -> evalWithEnv env old

eval' env val@(Atom _) = evalWithEnv env val

eval' _ n@(Number (Rational r))
  | denominator r == 1 = return (Number $ Integer $ numerator r)
  | otherwise = return n

eval' _ x = return x

-- attribute relating functions
attributeEvaluate :: (LispVal -> IOThrowsError LispVal) ->
  LispVal -> [LispVal] -> IOThrowsError [LispVal]
attributeEvaluate evalE h rests = do
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

attEvalOrderless :: [Attribute] -> [LispVal] -> [LispVal]
attEvalOrderless att vals
  | Orderless `elem` att = sort vals
  | otherwise = vals

attEvalFlatten :: [Attribute] -> LispVal -> [LispVal] -> [LispVal]
attEvalFlatten att h vals
  | Flatten `elem` att = deleteSameHead vals h
  | otherwise = vals

attEvalSeqHold :: [Attribute] -> [LispVal] -> [LispVal]
attEvalSeqHold att vals
  | SequenceHold `elem` att = vals
  | otherwise = deleteSameHead vals (Atom "Sequence")

allAttr :: [Attribute] -> LispVal-> [LispVal] -> [LispVal]
allAttr att h = attEvalOrderless att .attEvalFlatten att h .
                  attEvalSeqHold att
-- ----------------------------

deleteSameHead :: [LispVal] -> LispVal -> [LispVal]
deleteSameHead [] _ = []
deleteSameHead (val@(List x):xs) h
  | head x == h = tail x ++ deleteSameHead xs h
  | otherwise = val : deleteSameHead xs h
deleteSameHead (x:xs) h = x : deleteSameHead xs h


-- require eval function
primitives = M.fromList $ primitives' ++ requireEval

requireEval :: [(String,IOPrimi)]
requireEval = [
                -- ("Inequality",inequall)
              ]

-- inequality
-- inequall env xs =
--   let l = length xs in
--     if l >= 3 && odd l then do
--       res <- inequall' env xs
--       hasValue $ case res of
--         Atom _ -> res
--         List xs -> List (Atom "Inequality" : xs)
--     else
--       throwError (Default "Ineuqlity's number of arguments expected to be an odd number>=3")
--
-- inequall' :: Env -> [LispVal] -> IOThrowsError LispVal
-- inequall' env val@[a,comp,b] = do
--   res <- eval'  env $ List [comp,a,b]
--   return $ if isBool res then res else List val
-- inequall' env (a:comp:b:rest) = do
--   res <- eval' env $ List [comp,a,b]
--   if isBool res then
--     case res of
--       Atom "True" -> inequall' env (b:rest)
--       Atom "False" -> return res
--   else do
--     restRes <- inequall' env (b:rest)
--     return $ case restRes of
--       Atom "False" -> restRes
--       Atom "True" -> List [a,comp,b]
--       List xs -> List (a:comp:xs)
-- ---------------------------------------------------
-- logic :: (LispVal -> Bool) -> [LispVal] ->

-- ---------------------------------------------------
