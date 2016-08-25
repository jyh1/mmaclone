module Eval.Primitive.Compare.Compare
        (equall,lessl,lessEquall,greaterl,greaterEquall,inequalityl) where
import Data.DataType
import Data.Number.Number
import Eval.Primitive.PrimiFunc
import Data.Environment.EnvironmentType hiding(eval)

import Control.Monad
import Control.Monad.Except
import Data.Maybe
import qualified Data.Text as T


equall = comparel equal
lessl = comparel less
lessEquall = comparel lessEqual
greaterl = comparel greater
greaterEquall = comparel greaterEqual

comparel :: (Number -> Number -> Bool) -> Primi
comparel comp = usesArgumentMaybe (compareMaybe comp)

compareMaybe :: (Number -> Number -> Bool) -> [LispVal] -> Maybe LispVal
compareMaybe comp ls = do
  unpacked <- unpack ls
  return (toBool $ compareFunction comp unpacked)

compareFunction :: (Number -> Number -> Bool) -> [Number] -> Bool
compareFunction _ [] = True
compareFunction _ [_] = True
compareFunction comp (x1:x2:xs) =
  comp x1 x2 && compareFunction comp (x2:xs)

unpack :: [LispVal] -> Maybe [Number]
unpack = mapM unpacknum

unpacknum (Number a) = Just a
unpacknum _ = Nothing

-- inequalityl
compareNumber :: (Number -> Number -> Bool) -> LispVal -> LispVal -> Maybe Bool
compareNumber f x y = do
  x' <- unpacknum x
  y' <- unpacknum y
  return $ f x' y'

compareTable :: [(T.Text, LispVal -> LispVal -> Maybe Bool)]
compareTable = [
                ("Equal",compareNumber equal),
                ("Greater", compareNumber greater),
                ("GreaterEqual", compareNumber greaterEqual),
                ("Less", compareNumber less),
                ("LessEqual", compareNumber lessEqual),
                ("Unequal", compareNumber unequal)
                ]


eval :: LispVal -> LispVal -> LispVal -> Maybe Bool
eval (Atom name) x y = do
  f <- lookup name compareTable
  f x y

inequalityl :: Primi
inequalityl = usesArgumentError (lift . inequal)


inequal :: [LispVal] -> IOThrowsError LispVal
inequal xs =
  let l = length xs in
    if l >= 3 && odd l then do
      let res = inequalityl' xs
      return $ case res of
        Atom _ -> res
        List xs -> List (Atom "Inequality" : xs)
    else
      throwError (Default "Inequality's number of arguments expected to be an odd number >= 3")


-- eliminate left
inequalityRight :: [LispVal] -> LispVal
inequalityRight val@[a,comp,b] =
  maybe (List val) toBool (eval comp a b)
inequalityRight (a:comp:b:rest) =
  let res = eval comp a b
      restRes = inequalityRight (b:rest)
      check True = case restRes of
        val@(Atom _) -> val
        List xs -> List (a:comp:xs)
      check False = false
      checkRest (Atom "True") = List [a,comp,b]
      checkRest (Atom _) = false
      checkRest (List xs) = List (a:comp:xs)
      ifNothing = checkRest restRes
  in
    maybe ifNothing check res

-- eliminate right
inequalityl' :: [LispVal] -> LispVal
inequalityl' val@[a,comp,b] =
  maybe (List val) toBool (eval comp a b)
inequalityl' (a:comp:b:rest) =
  let res = eval comp a b
      left = inequalityRight (b:rest)
      goOn = inequalityl' (b:rest)
      check True = goOn
      check False = false
      checkRest (Atom "True") = List [a,comp,b]
      checkRest (Atom _) = false
      checkRest (List xs) = List (a:comp:xs)
      ifNothing = checkRest left
  in
    maybe ifNothing check res
