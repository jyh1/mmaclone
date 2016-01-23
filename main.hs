-- import           Text.ParserCombinators.Parsec
import Control.Monad
import Control.Monad.Except
import Data.IORef

import DataType
import Eval
import Parse


main :: IO()
main = do
  env <- nullEnv
  loop env

loop :: Env -> IO ()
loop env = do
  evaled <- runExceptT (repl env)
  report evaled
  loop env

repl :: Env -> IOThrowsError LispVal
repl env = do
  lift $ putStr "In:= "
  expr <- ExceptT (liftM readExpr getLine)
  eval env expr

report :: ThrowsError LispVal -> IO ()
report (Left err) = print err
report (Right None) = return ()
report (Right val) = putStrLn $ "Out=  " ++ show val
