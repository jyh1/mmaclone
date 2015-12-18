import           Text.ParserCombinators.Parsec
import Control.Monad

import DataType
import Eval
import Parse

main :: IO()
main = forever $
        getLine >>= (print . eval . readExpr)
