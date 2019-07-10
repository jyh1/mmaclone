module Eval.Primitive.List.Part(partl) where
import           Data.DataType
import           Data.Environment.EnvironmentType
import           Data.Number.Number
import           Eval.Primitive.PrimiFunc

import           Control.Monad
import           Control.Monad.Except
import           Data.Text                        (pack)
import qualified Data.Text.Internal

partl :: Primi
partl = do
  l <- getArgumentList
  expr <- getExpression
  case partWithPartError l of
    Left err  -> stateThrow $ fromPartError err expr
    Right val -> return val


data PartSpeci = S Int | L [Int] | All | Span PartSpeci PartSpeci -- deriving (Show)
data PartRes = Sres LispVal | Lres [LispVal]
data PartError = Pkspec | Partw | Partd  -- | Debug Data.Text.Internal.Text
type ThrowPart = Either PartError

partWithPartError :: [LispVal] -> ThrowPart LispVal
partWithPartError (l:ls) = do
  speci <- toPartSpeci ls
  partWithSpeci l speci

toPartSpeci :: [LispVal] -> ThrowPart [PartSpeci]
toPartSpeci = mapM toPartSpeci'
  where
    toPartSpeci' (List (Atom "List":res)) = liftM L $ mapM unpack res
    toPartSpeci' (List [Atom "Span",Atom "All",Atom "All"]) = Right All
    toPartSpeci' (List [Atom "Span",Atom "All",Number (Integer b)]) = Right $ Span All (S $ fromIntegral b)
    toPartSpeci' (List [Atom "Span",Number (Integer a),Atom "All"]) = Right $Span (S $ fromIntegral a) All
    toPartSpeci' (List [Atom "Span",Number (Integer a),Number (Integer b)]) = Right $ Span (S $ fromIntegral a) (S $ fromIntegral b)
    toPartSpeci' (Atom "All")             = Right All
    toPartSpeci' n                        = liftM S $ unpack n
    unpack :: LispVal -> ThrowPart Int
    unpack (Number (Integer n)) = return (fromIntegral n)
    unpack _                    = throwError Pkspec

partWithSpeci :: LispVal -> [PartSpeci] -> ThrowPart LispVal
partWithSpeci l [] = return l
partWithSpeci l (s:ss) = do
  parted <- partOnce l s
  case parted of
    Sres res   -> partWithSpeci res ss
    Lres reses -> liftM list $ mapM (`partWithSpeci` ss) reses

partOnce :: LispVal -> PartSpeci -> ThrowPart PartRes
partOnce lv (S n)  = liftM Sres $ getPart lv n
partOnce lv (L ns) = liftM Lres $ mapM (getPart lv) ns
partOnce lv@(List ls) All    = liftM Lres $ mapM (getPart lv) [1.. (-1+length ls)]
partOnce lv@(List ls) (Span All All)    = liftM Lres $ mapM (getPart lv) [1.. (-1+length ls)]
partOnce lv (Span All (S n))    = liftM Lres $ mapM (getPart lv) [1.. n]
partOnce lv (Span (S m) (S n))    = liftM Lres $ mapM (getPart lv) [m.. n]
partOnce lv@(List ls) (Span (S n) All)    = liftM Lres $ mapM (getPart lv) [n.. (-1+length ls)]
partOnce lv (Span _ _)    = liftM Lres $ throwError Pkspec
partOnce _ _    = liftM Lres $ throwError Partd




getPart :: LispVal -> Int -> ThrowPart LispVal
getPart (List lis) n =
  if length lis < n+1 then throwError Partw
    else return $ lis !! n
getPart _ _ = throwError Partd

fromPartError :: PartError -> LispVal -> LispError
fromPartError Partd = PartE "part specification is longer than depth of object"
fromPartError Partw = PartE "part specification does not exist"
fromPartError Pkspec = PartE "invalid part specification"
--fromPartError (Debug s) = PartE s
