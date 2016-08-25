module Eval.Primitive.IO.Print
        (printl) where

import Eval.Primitive.PrimiFunc
import Data.Environment.EnvironmentType
import Data.DataType
import Eval.Patt.Pattern
import Show.Pretty

import Control.Monad.Trans
import qualified Data.Text.IO as T

printl :: Primi
printl = do
  vals <- getArgumentList
  let output = mconcat $ map showLispVal vals in
    (lift.lift) $ T.putStrLn output >> return atomNull
