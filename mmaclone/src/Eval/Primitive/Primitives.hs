module Eval.Primitive.Primitives(primitives) where

import Eval.Primitive.Primi.Primi
import Eval.Primitive.IOPrimi.IOPrimi
import Eval.Primitive.PrimiType

import qualified Data.Map.Strict as M

primitives :: Primitives
primitives = M.fromList $ map (fmap toIOPrimi) primi ++ ioprimi
