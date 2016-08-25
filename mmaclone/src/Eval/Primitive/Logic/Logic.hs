module Eval.Primitive.Logic.Logic(andl, orl, notl) where

import Data.DataType

import Data.Number.Number
import Eval.Primitive.PrimiFunc
import Data.Environment.EnvironmentType
import Control.Monad
import Control.Monad.Trans.Class

data Logic = Result LispVal | NonReulst [LispVal]

unpackLogic :: LispVal -> Logic -> LispVal
unpackLogic _ (Result val) = val
unpackLogic h (NonReulst vals) = List (h: vals)

logic :: LispVal
   -> [LispVal] -> StateResult Logic
logic trivi [] = return (Result trivi)
logic trivi (x:xs) =
  let rest = logic trivi xs
      check = (trivi ==) in
  do
    x' <- evaluate x
    if isBool x' then
      if check x' then
        rest
      else
        return (Result x')
    else do
      restRes <- rest
      return $ case restRes of
        NonReulst res -> NonReulst (x':res)
        Result res -> if check res then
           NonReulst [x'] else restRes

logicLift :: LispVal -> Primi
logicLift triviality = do
  arguments <- getArgumentList
  h <- getHead
  fmap (unpackLogic h) (logic triviality arguments)

andl,orl,notl :: Primi
-- | short circut evaluation implemented
andl = logicLift true

orl = logicLift false

notl = do
  withnop 1
  usesArgumentMaybe notl'

notl' :: [LispVal] -> Maybe LispVal
notl' [Atom "True"] = Just false
notl' [Atom "False"] = Just true
notl' _ = Nothing
