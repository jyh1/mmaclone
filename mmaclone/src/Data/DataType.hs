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
import Text.Printf

-- * Module containing all foundamental types and functions
-- ** LispVal
data LispVal =
              Number Number
            | String String
            | Atom String
            | List [LispVal]
            | Char Char
  deriving(Eq, Ord)

type IOThrowsError = ExceptT LispError IO
-- type LispFun = LispVal -> IOThrowsError LispVal

instance Show LispVal where
  show = fullForm

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

fullForm :: LispVal -> String
fullForm (Atom s) = s
fullForm (List []) = ""
fullForm (List (l:ls)) =
  fullForm l ++ "[" ++ intercalate "," (map fullForm ls) ++ "]"
fullForm (Number i) = show i
fullForm (String s) = show s
-- fullForm (Char c) = show c

data Unpacker = forall a. Ord a => Unpacker (LispVal -> ThrowsError a)

-- data EqUnpacker = forall a. Eq a => EqUnpacker (LispVal -> ThrowsError a)

-- unpackNum' :: LispVal -> ThrowsError Number
unpackNum' (Number n) = return n
unpackNum' x = throwError $ TypeMismatch "number" x

unpackString' :: LispVal -> ThrowsError String
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

data LispError = NumArgs String Int Int
                | NumArgsMore String Int Int
                | NumArgsBetween String Int Int Int
                | TypeMismatch String LispVal
                | Parser ParseError
                | BadSpecialForm String LispVal
                | NotFunction String String
                | UnboundVar String String
                | Default String
                | PartE String LispVal
                | Incomplete [LispVal]
                | SetError LispVal
                | Level LispVal
                | SlotError LispVal
                | LimitExceed


instance Show LispError where
  show (UnboundVar message varname) = message ++ ": " ++ varname
  show (BadSpecialForm message form) = message ++ ": " ++ show form
  show (NotFunction message func) = message ++ ": " ++ show func
  show (NumArgs name expected found) = printf "%s is called with %d arguments, %d arguments are expected" name found expected
  show (NumArgsMore name botom found) = printf "%s is called with %d arguments, %d or more arguments are expected" name found botom
  show (NumArgsBetween name l r found) = printf "%s is called with %d arguments, between %d and %d arguments are exprected" name found l r
  show (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                        ++ ", found" ++ show found
  show (Parser parseErr) = "Parse error at " ++ show parseErr

  show (Incomplete s) = show s ++ "is incomplete.More input is needed"
  show (PartE tag v) = show v ++" "++ tag
  show (Default s) = s
  show (SetError v) = "Cannot assign to object " ++ show v
  show (Level v) = show v ++ " is not a valid level specification"
  show (SlotError s) = printf "%s cannot be fully filled" (show s)
  show LimitExceed = "Iteration Limit exceeded, try to increase $IterationLimit"

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
