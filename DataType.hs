module DataType
  (LispVal (..))
  where

import Data.List

data LispVal = Atom String
            | List [LispVal]
            | DottedList [LispVal] LispVal
            | Float Float
            | Number Integer
            | String String
            | Char Char
            | Bool Bool
            | None

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
