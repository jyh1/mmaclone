module Eval.Primitive.List.Map(mapl,applyl) where
import Eval.Primitive.List.Level
import Data.DataType
import Data.Number.Number
import Eval.Primitive.PrimiFunc
import Data.Environment.EnvironmentType


import Control.Monad
import Control.Monad.Except

mapl :: Primi
mapl = do
  between 2 3
  args <- getArgumentList
  lift (unpackArgs applyHead 1 args)

applyl :: Primi
applyl = do
  between 2 3
  args <- getArgumentList
  lift (unpackArgs changeHead 0 args)


mapl' = unpackArgs applyHead 1
applyl' = unpackArgs changeHead 0


unpackArgs :: (LispVal -> LispVal -> LispVal) -> Int ->
  [LispVal] -> IOThrowsError LispVal
unpackArgs fun def (f:app:speci) = do
  speciMap <- unpackNormalLevelSpeci def speci
  return (speciMap (fun f) app)
