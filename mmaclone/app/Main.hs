{-# LANGUAGE OverloadedStrings #-}
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
import Control.Lens
import qualified Data.Text as T
import qualified Data.Text.IO as T

info :: T.Text
info = T.unlines ["A simple Mathmatica clone (v0.1.0)",
                "Copyright Yonghao Jin (c) 2016.",
                "Contact me with jyh1@mail.ustc.edu.cn"]


lift2 = lift . lift

main :: IO()
main = do
  T.putStrLn info
  loop initialState

loop :: PrimiEnv -> IO ()
loop env = do
  res <- runExceptT $ execStateT repl env
  case res of
    Right newEnv ->
      loop newEnv
    Left err -> do
      print err
      loop env


getExpr :: IOThrowsError LispVal
getExpr = do
  string <- lift getLine
  liftThrows (readExpr string)

repl :: StateResult ()
repl = do
  n <- getLineNumber
  lift2 $ printf "In[%d]:= " n >> hFlush stdout
  expr <- lift getExpr
  res <- evalWithRecord expr
  new <- getLineNumber
  lift2 $ report new res
  line += 1

report :: Int -> LispVal -> IO ()
report _ (Atom "Null") = return ()
report n val = printf "Out[%d]= " n >> printLispVal val
