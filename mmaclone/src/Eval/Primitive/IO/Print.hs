module Eval.Primitive.IO.Print
        (printl) where

import Eval.Primitive.PrimiType
import Data.DataType
import Eval.Patt.Pattern
import Show.Pretty

import Control.Monad.Trans

printl :: Primi
printl = do
  vals <- getArgumentList
  let output = mconcat $ map showLispVal vals in
    (lift.lift) $ putStrLn output >> return atomNull
