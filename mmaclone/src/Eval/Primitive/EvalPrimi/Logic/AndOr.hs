module Eval.Primitive.EvalPrimi.Logic.AndOr(andl, orl) where

import Data.DataType

import Data.Number.Number
import Eval.Primitive.PrimiType
import Control.Monad

logic :: Eval
   -> LispVal
   -> [LispVal] -> IOThrowsError LispVal
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

logicLift _ val@(Atom _) = Just val
logicLift name val = Just (addHead (Atom name) val)

andl,orl :: EvalPrimi
-- | in order to achieve short circut evaluation,
-- require a eval function to evaluate arguments internally
andl eval ls =
  liftM (logicLift "And") $ logic eval true ls

orl eval ls =
  liftM (logicLift "Or") $ logic eval false ls
