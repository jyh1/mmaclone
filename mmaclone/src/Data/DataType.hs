{-#LANGUAGE ExistentialQuantification #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
module Data.DataType where

import Control.Monad.Except
-- import Data.IORef
import qualified Data.Map.Strict as M
import Control.Monad.Trans.Except
import           Text.ParserCombinators.Parsec(ParseError)
import Data.List
import Data.Number.Number
import qualified Data.Text as T
import Text.Printf

-- * Module containing all foundamental types and functions
-- ** LispVal
data LispVal =
              Number Number
            | String T.Text
            | Atom T.Text
            | List [LispVal]
            | Char Char
  deriving(Eq, Ord)

type IOThrowsError = ExceptT LispError IO
-- type LispFun = LispVal -> IOThrowsError LispVal

instance Show LispVal where
  show = T.unpack . fullForm

isNull :: LispVal -> Bool
isNull (Atom "Null") = True
isNull _ = False

atomNull = Atom "Null"

atomLine = Atom "$Line"

atomLimit = Atom "$IterationLimit"

atomIn = Atom "In"
atomOut = Atom "Out"

isBool (Atom "True") = True
isBool (Atom "False") = True
isBool _ = False

true = toBool True
false = toBool False

trueQ (Atom "True") = True
trueQ _ = False

toBool True = Atom "True"
toBool False = Atom "False"
unBool (Atom "True") = True
unBool (Atom "False") = False

list ls = List $ Atom "List" : ls

tshow :: (Show s) => s -> T.Text
tshow = T.pack . show

fullForm :: LispVal -> T.Text
fullForm (Atom s) = s
fullForm (List []) = ""
fullForm (List (l:ls)) =
  T.concat [fullForm l, "[", T.intercalate "," (map fullForm ls), "]"]
fullForm (Number i) = tshow i
fullForm (String s) = s
fullForm (Char c) = tshow c
-- fullForm (Char c) = show c

data Unpacker = forall a. Ord a => Unpacker (LispVal -> ThrowsError a)

-- data EqUnpacker = forall a. Eq a => EqUnpacker (LispVal -> ThrowsError a)

-- unpackNum' :: LispVal -> ThrowsError Number
unpackNum' (Number n) = return n
unpackNum' x = throwError $ TypeMismatch "number" x

unpackString' :: LispVal -> ThrowsError T.Text
unpackString' (String s) = return s
unpackString' x = throwError $ TypeMismatch "string" x

unpackChar' :: LispVal -> ThrowsError Char
unpackChar' (Char s) = return s
unpackChar' x = throwError $ TypeMismatch "string" x

unpackBool' :: LispVal -> ThrowsError Bool
unpackBool' (Atom "True") = return True
unpackBool' (Atom "False") = return False
unpackBool' x = throwError $ TypeMismatch "string" x

unpackers :: [Unpacker]
unpackers = [Unpacker unpackNum', Unpacker unpackString',
            Unpacker unpackChar',
            Unpacker unpackBool']

checkNum :: LispVal -> Bool
checkNum (Number _) = True
checkNum _ = False

unpackNum :: LispVal -> Number
unpackNum = extractValue . unpackNum'

integer :: (Integral a) => a -> LispVal
integer = Number . Integer . fromIntegral

double :: Double -> LispVal
double = Number . Double
-- ------------------------------------------

-- LispError

data LispError = NumArgs T.Text Int Int
                | NumArgsMore T.Text Int Int
                | NumArgsBetween T.Text Int Int Int
                | TypeMismatch T.Text LispVal
                | Parser ParseError
                | BadSpecialForm T.Text LispVal
                | NotFunction T.Text T.Text
                | UnboundVar T.Text T.Text
                | Default T.Text
                | PartE T.Text LispVal
                | Incomplete [LispVal]
                | SetError LispVal
                | Level LispVal
                | SlotError LispVal
                | LimitExceed


instance Show LispError where
  show = T.unpack . lispErrorToText

lispErrorToText :: LispError -> T.Text
lispErrorToText (UnboundVar message varname) = T.concat [message, ": ", varname]
lispErrorToText (BadSpecialForm message form) = T.concat [message, ": ", fullForm form]
lispErrorToText (NotFunction message func) = T.concat [message, ": ", func]
lispErrorToText (NumArgs name expected found) = T.pack (printf "%s is called with %d arguments, %d arguments are expected" name found expected)
lispErrorToText (NumArgsMore name botom found) = T.pack (printf "%s is called with %d arguments, %d or more arguments are expected" name found botom)
lispErrorToText (NumArgsBetween name l r found) = T.pack(printf "%s is called with %d arguments, between %d and %d arguments are exprected" name found l r)
lispErrorToText (TypeMismatch expected found) = T.concat ["Invalid type: expected ", expected,
                                      ", found", fullForm found]
lispErrorToText (Parser parseErr) = T.concat ["Parse error at ", tshow parseErr]

lispErrorToText (Incomplete s) = T.concat [fullForm (List s), "is incomplete.More input is needed"]
lispErrorToText (PartE tag v) = T.concat [fullForm v, " ", tag]
lispErrorToText (Default s) = s
lispErrorToText (SetError v) = T.concat ["Cannot assign to object ", fullForm v]
lispErrorToText (Level v) = T.concat [fullForm v, " is not a valid level specification"]
lispErrorToText (SlotError s) = T.pack (printf "%s cannot be fully filled" (fullForm s))
lispErrorToText LimitExceed = "Iteration Limit exceeded, try to increase $IterationLimit"

type ThrowsError = Either LispError

plusError :: ThrowsError a -> ThrowsError a -> ThrowsError a
plusError (Left _) l = l
plusError a _ = a

sumError :: [ThrowsError a] -> ThrowsError a
sumError = foldr plusError (Left (Default "mzero"))


trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- --------------------------------------------------

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

-- ---------------------------------
wrapSequence :: [LispVal] -> LispVal
wrapSequence xs = List (Atom "Sequence": xs)

applyHead,changeHead,addHead :: LispVal -> LispVal -> LispVal
applyHead h args = List [h,args]

changeHead h (List (l:ls)) = List (h:ls)
changeHead _ val = val

addHead h (List ls) = List (h:ls)
addHead _ _ = error "DataType.addHead :: Non list"

sortList :: LispVal -> LispVal
sortList (List (x:xs)) = List (x: sort xs)
sortList val = val

deleteSameHead :: [LispVal] -> LispVal -> [LispVal]
deleteSameHead [] _ = []
deleteSameHead (val@(List x):xs) h
  | head x == h = tail x ++ deleteSameHead xs h
  | otherwise = val : deleteSameHead xs h
deleteSameHead (x:xs) h = x : deleteSameHead xs h

unpackInt _ (Number (Integer n)) = return $ fromIntegral n
unpackInt err _ = throwError err

unpackIntWithThre thre err n = do
  n' <- unpackInt err n
  if n' < thre then throwError err else return n'

unpackAtom (Atom name) = name
unpackAtom _ = error "Data.DataType unpackAtom"
-- ----------------------------------------------
