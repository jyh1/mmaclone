{-#LANGUAGE ExistentialQuantification #-}
{-#LANGUAGE FlexibleInstances #-}
module DataType where

import Control.Monad.Except
import Data.IORef
import qualified Data.Map.Strict as M
import Control.Monad.Trans.Except
import           Text.ParserCombinators.Parsec(ParseError)
import Data.List
import Number

-- LispVal

data LispVal =
              Number Number
            | List [LispVal]
            | Atom String
            | String String
            | Char Char
  deriving(Eq, Ord)

instance Show LispVal where
  -- show (Atom s) = s
  -- show (List s) = '(' : (unwords $ map show s) ++ ")"
  -- show (Number i) = show i
  -- show (String s) = show s
  -- show (Char c) = show c
  show = fullForm

isNull :: LispVal -> Bool
isNull (Atom "Null") = True
isNull _ = False

atomNull = Atom "Null"

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
fullForm (Char c) = show c

data Unpacker = forall a. Ord a => Unpacker (LispVal -> ThrowsError a)

-- data EqUnpacker = forall a. Eq a => EqUnpacker (LispVal -> ThrowsError a)

unpackNum' :: LispVal -> ThrowsError Number
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
            Unpacker unpackChar', Unpacker unpackBool']

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

data LispError = NumArgs Integer [LispVal]
                | TypeMismatch String LispVal
                | Parser ParseError
                | BadSpecialForm String LispVal
                | NotFunction String String
                | UnboundVar String String
                | Default String
                | PartError LispVal LispVal
                | Incomplete [LispVal]


instance Show LispError where
  show (UnboundVar message varname) = message ++ ": " ++ varname
  show (BadSpecialForm message form) = message ++ ": " ++ show form
  show (NotFunction message func) = message ++ ": " ++ show func
  show (NumArgs expected found) = "Expected " ++ show expected ++
                                      " args: found values " ++ unwordsList found
    where unwordsList = unwords . map show
  show (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                        ++ ", found" ++ show found
  show (Parser parseErr) = "Parse error at " ++ show parseErr

  show (PartError vs n) = "part " ++ show n ++ "of " ++ show vs ++  "does not exist"

  show (Incomplete s) = show s ++ "is incomplete.More input is needed"
  show (Default s) = s

type ThrowsError = Either LispError

plusError :: ThrowsError a -> ThrowsError a -> ThrowsError a
plusError (Left _) l = l
plusError a _ = a

sumError :: [ThrowsError a] -> ThrowsError a
sumError = foldr plusError (Left (Default "mzero"))

type Result = ThrowsError (Maybe LispVal)

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- --------------------------------------------------

hasValue :: LispVal -> ThrowsError (Maybe LispVal)
hasValue = return . Just

noChange :: Result
noChange = return Nothing
-- --------------------------------
type SingleFun = LispVal -> Result
type BinaryFun = LispVal -> LispVal -> Result
-- ---------------------------------
type Pattern = LispVal
type Matched = (String, LispVal)
type Rule = (Pattern, LispVal)
type ValueRule = M.Map LispVal LispVal
type PatternRule = M.Map String [Rule]
data Context = Context {value :: ValueRule, pattern :: PatternRule}
-- IORef
type Env = IORef Context

nullEnv :: IO Env
nullEnv = newIORef (Context M.empty M.empty)

type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

setVar :: Env -> Pattern -> LispVal -> IOThrowsError LispVal
setVar envRef lhs rhs = liftIO $ do
  match <- readIORef envRef
  let newCont = setVar' match lhs rhs
  writeIORef envRef newCont
  return atomNull

setVar' :: Context -> Pattern -> LispVal -> Context
setVar' cont lhs rhs
  | isPattern lhs = Context values (insertPattern patterns lhs rhs)
  | otherwise = Context (insertRule values lhs rhs) patterns
    where patterns = pattern cont
          values = value cont

insertPattern :: PatternRule -> Pattern -> LispVal-> PatternRule
insertPattern rules lhs@(List (Atom name : _)) rhs =
  M.insertWith (++) name [(lhs,rhs)] rules

insertRule :: ValueRule -> LispVal -> LispVal -> ValueRule
insertRule rules lhs rhs =
  M.insert lhs rhs rules

isPattern :: LispVal -> Bool
isPattern (List (Atom "Pattern" : _)) = True
isPattern (List (Atom "Blank":_)) = True
isPattern (List xs) = any isPattern xs
isPattern _ = False

readRule :: Env -> IOThrowsError Context
readRule = liftIO . readIORef

wrapSequence :: [LispVal] -> LispVal
wrapSequence xs = List (Atom "Sequence": xs)
