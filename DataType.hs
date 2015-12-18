module DataType
  (LispVal (..))
  where

import Control.Monad.Except
import           Text.ParserCombinators.Parsec(ParseError)


data LispVal = Atom String
            | List [LispVal]
            | DottedList [LispVal] LispVal
            | Float Float
            | Number Integer
            | String String
            | Char Char
            | Bool Bool
            | None
  deriving(Eq)

instance Show LispVal where
  show (Atom s) = s
  show (List s) = '(' : (unwords $ map show s) ++ ")"
  show (DottedList a b) = init (show $ List a) ++ " . " ++ show b ++ ")"
  show (Float f) = show f
  show (Number i) = show i
  show (String s) = show s
  show (Char c) = show c
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show None = undefined


data LispError = NumArgs Integer [LispVal]
                | TypeMismatch String LispVal
                | Parser ParseError
                | BadSpecialForm String LispVal
                | NotFunction String String
                | UnboundVar String String
                | Defalut String

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
