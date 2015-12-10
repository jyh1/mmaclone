module DataType
  (LispVal (..))
  where

data LispVal = Atom String
            | List [LispVal]
            | DottedList [LispVal] LispVal
            | Float Float
            | Number Integer
            | String String
            | Char Char
            | Bool Bool
            | None
  deriving(Show)
