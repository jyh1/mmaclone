{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module NewParse
    (
    readExpr
    ) where

import           Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), many)
import Control.Monad.Except

import DataType
import Number

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^~#"

-- spaces :: Parser ()
-- spaces = skipMany space
--
-- spaces1 :: Parser ()
-- spaces1 = skipMany1 space

(<++>) a b = (++) <$> a <*> b
(<:>) a b = (:) <$> a <*> b

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

charW = lexeme . char

wrapBracket :: Char -> Char -> Parser a -> Parser a
wrapBracket a b p =
  charW a *> p <* charW b

brackets :: Parser a -> Parser a
brackets = wrapBracket '[' ']'

parsechar :: Parser Char
parsechar =
  let parseTab = '\t' <$ char 't'
      parseNewLine = '\n' <$ char 'n'
      ident = (char '\\' *>) in
    (ident $
            foldr1 (<|>)
              [char '\"', char '\\', parseTab, parseNewLine]) <|> noneOf "\""

enclose :: Parser a -> Parser b -> Parser b
enclose a = between a a

parseChar :: Parser LispVal
parseChar =
  Char <$> enclose (char '\'') parsechar

parseString :: Parser LispVal
parseString =
  String <$> enclose (char '\"') (many1 parsechar)

parseAtom :: Parser LispVal
parseAtom =
    fmap Atom parseAtomName

parseAtomName :: Parser String
parseAtomName =
  let first = letter
      rest = many (letter <|> digit) in
    first <:> rest


parseBlank :: Parser LispVal
parseBlank = do
  symbolName <- option "" parseAtomName
  underscore <- many1 (char '_')
  headName <- option "" parseAtomName
  let hN = if headName == "" then [] else [Atom headName]
      hasSymbol = List [Atom "Pattern", Atom symbolName, List (Atom blank : hN)]
      noSymbol = List (Atom blank : hN)
      blank = case underscore of
                "_" -> "Blank"
                "__" -> "BlankSequence"
                _ -> "BlankNullSequence"
  return $ if symbolName == "" then noSymbol else hasSymbol


-- parseAtom :: Parser LispVal
-- parseAtom =
--   let first = letter <|> symbol
--       rest = many (letter <|> digit <|> symbol) in
--     fmap check (first <:> rest)
--       where check "#t" = Bool True
--             check "#f" = Bool False
--             check atom = Atom atom

parseNumber :: Parser LispVal
parseNumber = do
  let number = many1 digit
      plus = char '+' *> number
      minus = char '-' <:> number
      integer = plus <|> minus <|> number
      decimal = option "" $ char '.' <:> number
      expo = option "" $ oneOf "eE" <:> integer

  int <- integer
  deci <- decimal <++> expo
  return $ check int deci
    where check int "" = Number $ Integer (read int)
          check int deci = Number $ Double (read (int ++ deci))

argList :: Parser [LispVal]
argList = brackets (sepBy (lexeme parseExpr) (charW ','))


-- parseList :: Parser LispVal
-- parseList = do
--   heads <- lexeme parseAtom
--   args <- option
--   return (List $ heads : args)

atomOrList :: Parser LispVal
atomOrList = do
  heads  <- lexeme parseAtom
  let attachHead ls = List $ heads : ls
  fmap attachHead argList <|> return heads

-- parseQuoted :: Parser LispVal
-- parseQuoted =
--   let quoted = char '\'' *> parseExpr in
--     (\x -> List [Atom "quote", x]) <$> quoted

parseExpr :: Parser LispVal
parseExpr = try parseNumber
            <|> parseString
            <|> try parseChar
            <|> try parseBlank
            <|> atomOrList


readExpr :: String -> ThrowsError LispVal
readExpr input =
  case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val
