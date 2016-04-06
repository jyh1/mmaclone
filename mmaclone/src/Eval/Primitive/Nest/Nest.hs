{-#LANGUAGE FlexibleContexts #-}
module Eval.Primitive.EvalPrimi.Nest.Nest(nestl, nestListl) where

import Data.DataType
import Data.Number.Number
import Eval.Primitive.PrimiType
import Eval.Primitive.Primi.Replace.Replace

import Control.Monad
import Control.Monad.Except

nestl ,nestListl:: EvalPrimi
nestl eval = withnop 3 "Nest" (nestl' eval)
nestListl eval = withnop 3 "NestList" (nestListl' eval)


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

-- nestUnpack :: (Eval -> LispVal -> LispVal -> Int -> EvalResult)
--    -> Eval -> [LispVal] -> IOResult
nestUnpack nest eval [f,arg,n] = do
  n' <- unpackIntWithThre 0 nestErr n
  liftM Just $ nest eval f arg n'

nestl' = nestUnpack nest

nestListl' = nestUnpack nestList
