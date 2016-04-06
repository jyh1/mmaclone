module Eval.Primitive.IOPrimi.IO.Print
        (printl) where

import Eval.Primitive.PrimiType
import Data.DataType
import Eval.Patt.Pattern
import Show.Pretty

import Control.Monad.Trans

printl :: IOPrimi
printl _ vals =
  let output = mconcat $ map showLispVal vals in
    lift $ putStrLn output >> hasValue atomNull
