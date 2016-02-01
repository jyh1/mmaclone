module Parser (
  parseExpr
) where

-- import Syntax

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Data.Functor.Identity

import DataType

langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
  { Tok.commentStart    = "(*"
  , Tok.commentEnd      = "*)"
  , Tok.commentLine     = "--"
  , Tok.nestedComments  = True
  , Tok.identStart      = letter
  , Tok.identLetter     = alphaNum
  , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.reservedNames   = words "True False"
  , Tok.reservedOpNames = words "+ -"
  , Tok.caseSensitive   = True
  }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

prefixOp :: String -> (a -> a) -> Ex.Operator String () Identity a
prefixOp s f = Ex.Prefix (reservedOp s >> return f)

-- binaryOp :: String -> (a -> a -> a) -> Ex.Assoc -> Ex.Operator String () Identity a
-- binaryOp s f ass = Ex.Infix ()

brackets = Tok.brackets lexer
indentifier = Tok.identifier lexer



-- Prefix operators
table :: Ex.OperatorTable String () Identity LispVal
table = [
    [
      prefixOp "!" id
    , prefixOp "pred" id
    , prefixOp "iszero" id
    ]
  ]

function :: Parser LispVal
function = do
  heads <- expr
  args <- brackets $ commaSep expr
  return (List (heads:args))


-- Constants
true, false,bool:: Parser LispVal
true  = reserved "True"  >> return (Bool True)
false = reserved "False" >> return (Bool False)
bool = true <|> false

variable :: Parser LispVal
variable = Atom <$> indentifier

expr :: Parser LispVal
expr = Ex.buildExpressionParser table factor

factor :: Parser LispVal
factor =
  bool
  <|> try variable
  -- <|> zero
  <|> function
  <|> parens expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [LispVal]
toplevel = semiSep expr

parseExpr :: String -> Either ParseError LispVal
parseExpr s = parse (contents expr) "<stdin>" s
