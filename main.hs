-- import           Text.ParserCombinators.Parsec
import Control.Monad
import Control.Monad.Except
import Text.Printf

import DataType
import Eval
import Parse
-- import Pretty

main :: IO()
main = do
  env <- nullEnv
  loop env 1

loop :: Env  -> Int -> IO ()
loop env n = do
  evaled <- runExceptT (repl env n)
  report n evaled
  loop env (n + 1)

repl :: Env -> Int -> IOThrowsError LispVal
repl env n = do
  lift $ printf "In[%d]:=" n
  expr <- ExceptT (liftM readExpr getLine)
  evalWithRecord env n expr

report :: Int -> ThrowsError LispVal -> IO ()
report _ (Left err) = print err
report _ (Right None) = return ()
report n (Right val) = printf "Out[%d]=" n >> putStrLn (show val)
