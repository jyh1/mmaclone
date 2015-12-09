-- {-# LANGUAGE NoMonomorphismRestriction #-}
import           Text.ParserCombinators.Parsec hiding (spaces)

-- import           System.Environment

import           Control.Monad

import Control.Applicative hiding ((<|>), many)

data LispVal = Atom String
              | List [LispVal]
              | DottedList [LispVal] LispVal
              | Flaot Float
              | Number Integer
              | String String
              | Char Char
              | Bool Bool
  deriving(Show)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

-- readExpr :: String -> String
-- readExpr input =
--   case parse (spaces >> parseExpr) "lisp" input of
--     Left err -> "No match: "++ show err
--     Right (String str) -> str
--     Right _ -> "Found Value"

spaces :: Parser ()
spaces = skipMany space

(<++>) a b = (++) <$> a <*> b
(<:>) a b = (:) <$> a <*> b

parsechar :: Parser Char
parsechar =
  let parseTab = '\t' <$ char 't'
      parseNewLine = '\n' <$ char 'n'
      ident = (char '\\' *>) in
    (ident $
            foldr1 (<|>)
              [char '\"', char '\\', parseTab, parseNewLine]) <|> noneOf "\""

enclose :: Parser a -> Parser b -> Parser b
enclose a b =
  a *> b <* a

parseChar :: Parser LispVal
parseChar =
  fmap Char $ enclose (char '\'') parsechar

parseString :: Parser LispVal
parseString =
  fmap String $ enclose (char '\"') (many1 parsechar)

parseAtom :: Parser LispVal
parseAtom =
  let first = letter <|> symbol
      rest = many (letter <|> digit <|> symbol) in
    fmap check $ (first <:> rest)
      where check "#t" = Bool True
            check "#f" = Bool False
            check atom = Atom atom


-- number = many1 digit
-- plus = char '+' *> number
-- minus = char '-' <:> number
-- integer = plus <|> minus <|> number

parseNumber :: Parser LispVal
parseNumber = (Number . read) <$> many1 digit


parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber <|> parseChar


main :: IO()
main = forever $
        getLine >>= parseTest (spaces >> parseExpr)
