module Main where
import Control.Monad
import Control.Monad.Except
import Text.Printf
import System.IO

import Data.DataType
import Data.Environment.Environment
import Eval.Eval
import Parser.Trans
import Show.Pretty

info :: String
info = unlines ["A simple Mathmatica clone (v0.1.0)",
                "Copyright Author Yonghao Jin here (c) 2016.",
                "Contact me with jyh1@mail.ustc.edu.cn"]

main :: IO()
main = do
  env <- nullEnv
  putStrLn info
  loop env 1

loop :: Env  -> Int -> IO ()
loop env n = do
  evaled <- runExceptT (repl env n)
  report n evaled
  loop env (n + 1)

repl :: Env -> Int -> IOThrowsError LispVal
repl env n = do
  lift $ printf "In[%d]:= " n >> hFlush stdout
  expr <- ExceptT (liftM readExpr getLine)
  evalWithRecord env n expr

report :: Int -> ThrowsError LispVal -> IO ()
report _ (Left err) = print err
report _ (Right (Atom "Null")) = return ()
report n (Right val) =  printLispVal val
-- printf "Out[%d]= " n >>
