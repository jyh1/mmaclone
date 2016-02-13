module Eval.Primitive.Primi.Compare.Compare
        (equall,lessl,lessEquall,greaterl,greaterEquall,inequalityl) where
import Data.DataType
import Data.Number.Number
import Eval.Primitive.PrimiType

import Control.Monad
import Control.Monad.Except
import Data.Maybe

equall = comparel equal
lessl = comparel less
lessEquall = comparel lessEqual
greaterl = comparel greater
greaterEquall = comparel greaterEqual

comparel :: (Number -> Number -> Bool) -> Primi
comparel comp ls = return $ do
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

compareTable :: [(String,LispVal -> LispVal -> Maybe Bool)]
compareTable = [
                ("Equal",compareNumber equal),
                ("Greater", compareNumber greater),
                ("GreaterEqual", compareNumber greaterEqual),
                ("Less", compareNumber less),
                ("LessEqual", compareNumber lessEqual)
                ]


eval :: LispVal -> LispVal -> LispVal -> Maybe Bool
eval (Atom name) x y = do
  f <- lookup name compareTable
  f x y


inequalityl :: Primi
inequalityl xs =
  let l = length xs in
    if l >= 3 && odd l then do
      let res = inequalityl' xs
      hasValue $ case res of
        Atom _ -> res
        List xs -> List (Atom "Inequality" : xs)
    else
      throwError (Default "Inequality's number of arguments expected to be an odd number>=3")


inequalityl' :: [LispVal] -> LispVal
inequalityl' val@[a,comp,b] =
  maybe (List val) toBool (eval comp a b)
inequalityl' (a:comp:b:rest) =
  let res = eval comp a b
      restRes = inequalityl' (b:rest)
      check True = restRes
      check False = false
      checkRest (Atom "True") = List [a,comp,b]
      checkRest (Atom _) = false
      checkRest (List xs) = List (a:comp:xs)
      ifNothing = checkRest restRes
  in
    maybe ifNothing check res
