module Eval.Primitive.Primi.List.Map(mapl) where
import Eval.Primitive.Primi.List.Level
import Data.DataType
import Data.Number.Number
import Eval.Primitive.PrimiType


import Control.Monad
import Control.Monad.Except

mapl = manynop "Map" 2 3 mapl'

data Args = Args LispVal LispVal Int Int

mapl' args = do
  args' <- unpackArgs args
  hasValue $ mapTo args'


mapTo :: Args -> LispVal
mapTo (Args fun app low upper) =
  levelMapFromTo (applyTo fun) low upper app


unpack :: LispVal -> LispVal -> ThrowsError Int
unpack _ (Number (Integer n)) = return (fromIntegral n)
unpack val _ = throwError $ Level val

unpackArgs :: [LispVal] -> ThrowsError Args
unpackArgs [f,app] = return $ Args f app 1 1
unpackArgs [f,app,val@(List [Atom "List",n])] = do
  n' <- unpack val n
  return $ Args f app n' n'
unpackArgs [f, app, val@(List [Atom "List", i,j])] = do
  let unpack' = unpack val
  i' <- unpack' i
  j' <- unpack' j
  return $ Args f app i' j'
unpackArgs [f, app ,n] = do
  n' <- unpack n n
  return $ Args f app 1 n'
