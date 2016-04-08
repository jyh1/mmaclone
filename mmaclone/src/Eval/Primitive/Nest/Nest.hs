{-#LANGUAGE FlexibleContexts #-}
module Eval.Primitive.Nest.Nest(nestl, nestListl)where

import Data.DataType
import Data.Number.Number
import Eval.Primitive.PrimiType
import Eval.Primitive.Replace.Replace

import Control.Monad
import Control.Monad.Except

nestl ,nestListl:: Primi
nestl  = nestUnpack nest
nestListl = nestUnpack nestList

type Nest = Eval -> LispVal -> LispVal -> Int -> IOThrowsError LispVal

nest,nestList :: Nest
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

nestUnpack :: Nest -> Primi
nestUnpack nest = do
  withnop 3
  [f,arg,n] <- getArgumentList
  eval <- getEval
  lift $ do
    n' <- unpackIntWithThre 0 nestErr n
    nest eval f arg n'
