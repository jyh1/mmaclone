{-#LANGUAGE FlexibleContexts #-}

module Eval.Lambda(evalLambda,functionl) where

import Data.DataType
import Eval.Patt.Pattern
import Eval.Primitive.PrimiType

import Control.Monad.Trans.Except
import Control.Monad.Except


slotErr = Default "Slot should contain a non-negative integer"
slotSeqErr = Default "SlotSequence should contain a positive integer"
fpct = Default "Function:: Too many parameters to be filled"
flpr = Default "Function :: Parameter specification error"

unpackSlotNum,unpackSlotSeqNum :: LispVal -> ThrowsError Int
unpackSlotNum = unpackIntWithThre 0 slotErr
unpackSlotSeqNum = unpackIntWithThre 1 slotSeqErr

unpackSlot vs [n] = do
  n' <- unpackSlotNum n
  if length vs < n'+1 then
    throwError (SlotError (head vs))
  else return $ vs !! n'
unpackSlot  _ other = throwError (NumArgs "Slot" 1 other)

unpackSlotSeq vs [n] = do
  n' <- unpackSlotSeqNum n
  if length vs < n' then
    throwError (SlotError (head vs))
  else return $ wrapSequence (drop n' vs)
unpackSlotSeq _ other = throwError (NumArgs "SlotSequence" 1 other)


replaceSlot :: [LispVal] -> LispVal -> ThrowsError LispVal
replaceSlot vs (List val@(Atom "Slot":inds)) =
  unpackSlot vs inds
replaceSlot vs (List val@(Atom "SlotSequence":inds)) =
  unpackSlotSeq vs inds
replaceSlot _ val@(List (Atom "Function":_)) = return val
replaceSlot vs (List lis) =
  liftM List $ mapM (replaceSlot vs) lis
replaceSlot _ val = return val

unpackPara :: LispVal -> [LispVal] -> ThrowsError [Matched]
unpackPara (Atom _) [] = throwError fpct
unpackPara (Atom name) vals = return [(name,head vals)]
unpackPara (List (Atom "List":paras)) vals =
  if length paras > length vals then
    throwError fpct
  else
    return $ zip (map unpackAtom paras) vals

replaceVar paras args body = do
  matched <- unpackPara paras args
  return $ internalReplace body matched

evalLambda :: LispFun
evalLambda (List lis@(List fun:args)) =
  liftThrows $ case fun of
    [_,slots] -> replaceSlot lis slots
    _:para:body:_ -> replaceVar para args body


functionl :: Primi
functionl = manynop "Function" 1 3 checkFunction

checkFunction [_] = noChange
checkFunction (Atom _ :_) = noChange
checkFunction (List (Atom "List":_) :_) = noChange
checkFunction _ = throwError flpr
