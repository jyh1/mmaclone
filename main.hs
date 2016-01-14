import           Text.ParserCombinators.Parsec
import Control.Monad
import Control.Monad.Except

import DataType
import Eval
import Parse

main :: IO()
main = forever $
  runExceptT repl >>= report

repl :: ExceptT LispError IO LispVal
repl = do
  lift $ putStr "In:= "
  expr <- ExceptT (liftM readExpr getLine)
  ExceptT $ return  (eval expr)

report :: ThrowsError LispVal -> IO ()
report (Left err) = print err
report (Right val) = putStrLn $ "Out=  " ++ show val
