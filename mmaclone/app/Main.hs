{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Monad
import Control.Monad.Except
import Text.Printf
import System.IO
import Control.Monad.Trans.State

import Data.DataType
import Data.Environment.Environment
import Data.Environment.EnvironmentType
import Eval.Eval
import Eval.Primitive.PrimiFunc
import Parser.Trans
import Show.Pretty

import Control.Lens
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Console.Haskeline


info :: T.Text
info = T.unlines ["A simple Mathmatica clone (v0.1.0)",
                "Copyright Yonghao Jin (c) 2016.",
                "Feel free to contact me via jyh1@mail.ustc.edu.cn"]


main :: IO()
main = do
  T.putStrLn info
  runInputT defaultSettings (loop initialState)


loop :: PrimiEnv -> InputT IO ()
loop env =
  let cl = env ^. line in
    do
      input <- getInputLine (printf "In[%d]:= " cl :: String)
      case input of
        Just input ->
          repl env input
        Nothing -> return ()



repl :: PrimiEnv -> String -> InputT IO ()
repl env input = do
  res <- lift (runExceptT $ runStateT (evaluateExpression input) env)
  case res of
    Right (ans, newEnv) ->
      let ncl = newEnv ^. line in
        do
          when (ans /= "") $ outputStrLn ans 
          loop newEnv
    Left err -> do
      outputStrLn (show err)
      loop env



type Repl = InputT (StateT PrimiEnv IOThrowsError) ()

getExpr :: String -> IOThrowsError LispVal
getExpr string =
  liftThrows (readExpr string)


evaluateExpression :: String -> StateResult String
evaluateExpression str = do
  expr <- lift (getExpr str)
  res <- evalWithRecord expr
  new <- getLineNumber
  line += 1
  return (report new res)


report :: Int -> LispVal -> String
report _ (Atom "Null") = ""
report n val = printf "Out[%d]= " n ++ T.unpack (showLispVal val)
