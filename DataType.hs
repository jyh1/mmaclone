{-#LANGUAGE ExistentialQuantification #-}
{-#LANGUAGE FlexibleInstances #-}
module DataType where

import Control.Monad.Except
import Data.IORef
import Control.Monad.Trans.Except
import           Text.ParserCombinators.Parsec(ParseError)
import Number

-- LispVal

data LispVal = Atom String
            | List [LispVal]
            | DottedList [LispVal] LispVal
            | Number Number
            | String String
            | Char Char
            | Bool Bool
            | None
  deriving(Eq, Ord)

instance Show LispVal where
  show (Atom s) = s
  show (List s) = '(' : (unwords $ map show s) ++ ")"
  show (DottedList a b) = init (show $ List a) ++ " . " ++ show b ++ ")"
  show (Number i) = show i
  show (String s) = show s
  show (Char c) = show c
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show None = ""

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
unpackBool' (Bool s) = return s
unpackBool' x = throwError $ TypeMismatch "string" x

unpackers :: [Unpacker]
unpackers = [Unpacker unpackNum', Unpacker unpackString',
            Unpacker unpackChar', Unpacker unpackBool']

checkNum :: LispVal -> Bool
checkNum (Number _) = True
checkNum _ = False

unpackNum :: LispVal -> Number
unpackNum = extractValue . unpackNum'

integer :: Integer -> LispVal
integer n = Number $ Integer n


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
-- IORef
type Env = IORef [Rule]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

setVar :: Env -> Pattern -> LispVal -> IOThrowsError LispVal
setVar envRef lhs rhs = liftIO $ do
  match <- readIORef envRef
  writeIORef envRef ((lhs,rhs): match)
  return None

readRule :: Env -> IOThrowsError [Rule]
readRule = liftIO . readIORef
