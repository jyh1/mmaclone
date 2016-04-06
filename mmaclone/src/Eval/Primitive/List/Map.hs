module Eval.Primitive.Primi.List.Map(mapl,applyl) where
import Eval.Primitive.Primi.List.Level
import Data.DataType
import Data.Number.Number
import Eval.Primitive.PrimiType


import Control.Monad
import Control.Monad.Except

mapl = manynop "Map" 2 3 mapl'
applyl = manynop "Apply" 2 3 applyl'


mapl' = unpackArgs applyHead 1
applyl' = unpackArgs changeHead 0


unpackArgs :: (LispVal -> LispVal -> LispVal) -> Int ->
  [LispVal] -> Result
unpackArgs fun def (f:app:speci) = do
  speciMap <- unpackNormalLevelSpeci def speci
  hasValue (speciMap (fun f) app)
  -- hasValue $ levelMap (fun f)  defa app
-- unpackArgs fun _ [f,app,val@(List [Atom "List",n])] = do
--   n' <- unpack val n
--   hasValue $ levelMap (fun f) n' app
-- unpackArgs fun _ [f, app, val@(List [Atom "List", i,j])] = do
--   let unpack' = unpack val
--   i' <- unpack' i
--   j' <- unpack' j
--   hasValue $ levelMapFromTo (fun f) i' j' app
-- unpackArgs fun _ [f, app ,n] = do
--   n' <- unpack n n
--   hasValue $ levelMapUpTo (fun f) n' app
