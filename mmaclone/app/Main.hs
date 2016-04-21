module Main where
import Control.Monad
import Control.Monad.Except
import Text.Printf
import System.IO
import Control.Monad.Trans.State

import Data.DataType
import Data.Environment.Environment
import Eval.Eval
import Eval.Primitive.PrimiType
import Parser.Trans
import Show.Pretty

info :: String
info = unlines ["A simple Mathmatica clone (v0.1.0)",
                "Copyright Author Yonghao Jin here (c) 2016.",
                "Contact me with jyh1@mail.ustc.edu.cn"]

main :: IO()
main = do
  putStrLn info
  loop initialState 1

loop :: PrimiEnv -> Int -> IO ()
loop env n = do
  res <- runExceptT (repl env n)
  case res of
    Right (res, newEnv) -> do
      report n res
      loop newEnv (n+1)
    Left err -> do
      print err
      loop env (n+1)


getExpr :: IOThrowsError LispVal
getExpr = do
  string <- lift getLine
  liftThrows (readExpr string)

repl :: PrimiEnv -> Int -> IOThrowsError (LispVal, PrimiEnv)
repl env n = do
  lift $ printf "In[%d]:= " n >> hFlush stdout
  expr <- getExpr
  runStateT (evalWithRecord n expr) env

report :: Int -> LispVal -> IO ()
report _ (Atom "Null") = return ()
report n val =  printLispVal val
-- printf "Out[%d]= " n >>
