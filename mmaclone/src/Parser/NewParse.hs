{-# LANGUAGE OverloadedStrings #-}
module Parser.NewParse(Expr(..), parseExpr,Stage1,expr) where

import Text.Parsec hiding (Empty)
import Text.Parsec.String
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language
-- import Control.Applicative((*>))
import Text.Parsec.Expr

import Data.Number.Number
import qualified Data.Text as T

data Expr
  = Num Number
   | Lis [Expr]
   | Args [Expr]
   | Var T.Text
   | Add Expr Expr
  --  | Sub Expr Expr
   | Mul Expr Expr
   | Pow Expr Expr
  --  | Div Expr Expr
   | Inverse Expr
  --  | Mod Expr Expr
   | And Expr Expr
   | Or Expr Expr
   | Not Expr
   | Equal Expr Expr
   | Less Expr Expr
   | LessEq Expr Expr
   | Great Expr Expr
   | GreatEq Expr Expr
   | UnEq Expr Expr
   | Compound Expr Expr-- Expr; Expr
   | Apply Expr Expr
   | Fact Expr
   | Fact2 Expr
   | Negate Expr
   | Part Expr Expr
   | PartArgs [Expr]
   | Map Expr Expr
   | MapAll Expr Expr
   | Apply1 Expr Expr
   | Apply11 Expr Expr
   | Derivative Int Expr
   | Rule Expr Expr
   | RuleDelayed Expr Expr
   | Replace Expr Expr
   | ReplaceRepeated Expr Expr
   | Set Expr Expr
   | SetDelayed Expr Expr
   | Unset Expr
   | Dot Expr Expr
   | Blk
   | BlkE Expr
   | BlkSeq
   | BlkSeqE Expr
   | NullSeq
   | NullSeqE Expr
   | Pattern Expr Expr
   | PatternTest Expr Expr
   | Function Expr
   | Slot Int
   | SlotSeq Int
   | Str T.Text
   | Chr Char
   | Out Int
   | None
   | Cond Expr Expr
   | Alter Expr Expr
   deriving (Show,Eq)

opNames = words ("-> :> && || ! + - * / ; == < <= > >= : @ @@ /@ //@ @@@ \' !! != /. //. = :="
                  ++ " // & ? *) (* !! /; : | ^" )-- reserved operations

lexerConfig = Token.LanguageDef { Token.commentStart = "(*" -- adding comments is easy
                      , Token.commentEnd = "*)"
                      , Token.commentLine = ""
                      , Token.identStart = letter <|> char '$' -- identifiers must start with a letter
                      , Token.identLetter = alphaNum
                      , Token.reservedNames = []
                      , Token.reservedOpNames = opNames
                      , Token.opLetter = oneOf "@/=.>!;&"
                      , Token.caseSensitive = True
                      , Token.nestedComments = False
                      , Token.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~"
                      }

lexer = Token.makeTokenParser lexerConfig

identifier = Token.identifier lexer -- parses a valid identifier in our language
symbol     = Token.symbol lexer     -- parses a symbol like "]"
reserved   = Token.reserved lexer   -- parses a reserved word like "If"
reservedOp = Token.reservedOp lexer -- parses a reserved operation like "<="
parens     = Token.parens lexer     -- parses parenthesis surrounding the parser passed to it
brackets   = Token.brackets lexer   -- parses brackets surrounding the parser passed to it
braces = Token.braces lexer
commaSep   = Token.commaSep lexer   -- parses some or no comma separated instances of
                                    -- the argument parser
integer    = Token.integer lexer    -- parses an integer
whiteSpace = Token.whiteSpace lexer -- parses whitespace

naturalOrFloat = Token.naturalOrFloat lexer

stringLiteral = Token.stringLiteral lexer

charLiteral = Token.charLiteral lexer

natural = Token.natural lexer

lexeme = Token.lexeme lexer

semi = Token.semi lexer

prefix name label = Prefix (reservedOp name *> return label)

binary name label assoc = Infix (do{ reservedOp name
                                   ; return label
                                   }) assoc

postfix name label = Postfix (reservedOp name *> return label)

opTable = [
            [derivative],
            -- [function],
            [binary "?" PatternTest AssocRight],
            [appl,applPart],
            -- [postfix "&" Function],
            [binary "@" uniapply AssocRight],
            [ binary "/@" Map AssocRight,
              binary "//@" MapAll AssocRight,
              binary "@@" Apply1 AssocRight,
              binary "@@@" Apply11 AssocRight
            ],
            -- [derivative],

            [postfix "!" Fact,
            postfix "!!" Fact2],

            [binary "^" Pow AssocRight],
            [binary "." Dot AssocLeft],
            [ binary "*" Mul AssocLeft
            , binary "/" divide AssocLeft
            -- , binary "%" Mod AssocLeft,
            , spaceMul ]
          , [ binary "+" Add AssocLeft
            , binary "-" sub AssocLeft
            ],
            [prefix "-" Negate]
          , [ binary "==" Equal AssocLeft
            , binary "<" Less AssocLeft
            , binary "<=" LessEq AssocLeft
            , binary ">" Great AssocLeft
            , binary ">=" GreatEq AssocLeft
            , binary "!=" UnEq AssocLeft
            ]
          , [prefix "!" Not]
          , [ binary "&&" And AssocLeft ]
          , [ binary "||" Or AssocLeft ]
          , [binary "|" Alter AssocLeft]
          , [binary ":" Pattern AssocLeft]
          , [binary "/;" Cond AssocLeft]
          , [binary "->" Rule AssocRight,
            binary ":>" RuleDelayed AssocRight]
          , [binary "/." Replace AssocLeft,
            binary "//." ReplaceRepeated AssocLeft]
          -- , [function]
          , [postfix "&" Function]
          , [binary "//" (flip uniapply) AssocLeft]
          , [binary "=" Set AssocRight,
            binary ":=" SetDelayed AssocRight,
            postfix "=." Unset]

          , [binary ";" Compound AssocLeft]
          -- , [appl,binary "@" uniapply AssocRight]
          ]

sub e1 e2 = Add e1 (Negate e2)
divide e1 e2 = Mul e1 (Inverse e2)

uniapply h a = Apply h (Args [a])

appl = Infix space AssocLeft
    where space = whiteSpace
            *> lookAhead (char '[')
            *> notFollowedBy (string "[[")
            *> return Apply


function = Postfix $
  symbol "&" *> notFollowedBy (char '&') *> return Function

applPart = Infix space AssocLeft
    where space = whiteSpace
            *> lookAhead (symbol "[[")
            *> return Part


spaceMul = Infix space AssocLeft
      where space = whiteSpace
              *> notFollowedBy (choice . map reservedOp $ ("[":opNames))
              *> return Mul

derivative = Postfix $ do
                        ps <- many1 (char '\'')
                        return (Derivative (length ps))

opExpr :: Parser Expr
opExpr = buildExpressionParser opTable term

(<++>) a b = (++) <$> a <*> b
(<:>) a b = (:) <$> a <*> b

sign ::Parser (Expr -> Expr)
sign = (symbol "-" >> return Negate)
        <|> (symbol "+" >> return id)
        <|> return id

number :: Parser Expr
number = do
  num <- naturalOrFloat
  let numE = case num of
              Left a  -> Integer a
              Right b  -> Double b
  -- return (s $ Number numE)
  return (Num numE)

list :: Parser Expr
list = Lis <$> braces (commaSep expr)


argument :: Parser Expr
argument =
  Args <$> brackets (commaSep expr)

partArgs :: Parser Expr
partArgs =
  PartArgs <$> between (symbol "[[") (symbol "]]") (commaSep expr)

var :: Parser Expr
var = (Var . T.pack) <$> identifier

stringE :: Parser Expr
stringE = (Str . T.pack) <$> stringLiteral

charE :: Parser Expr
charE = Chr <$> charLiteral
-- special form -------------------
atomName :: Parser Expr
atomName = do
  c <- letter
  cs <- many alphaNum
  return $ (Var . T.pack) (c:cs)

blk :: Parser Expr
blk = string "_" *> return Blk

blkSeq :: Parser Expr
blkSeq = string "__" *> return BlkSeq

blkNullSeq :: Parser Expr
blkNullSeq = string "___" *> return NullSeq

blankE :: Parser Expr -> (Expr -> Expr) -> Parser Expr
blankE p f = do
  p
  name <- atomName
  return (f name)

pattern :: Parser Expr -> Parser Expr
pattern p = do
  name <- atomName
  blk <- p
  return (Pattern name blk)


blks = [blk, blkSeq, blkNullSeq]

-- blanks = zipWith blank blks [Blk, BlkSeq, NullSeq]
blankEs = zipWith blankE blks [BlkE, BlkSeqE, NullSeqE]
patternBlankEs = map pattern blankEs
patternBlanks = map pattern blks

specialForms =
  let forms = map try (patternBlankEs ++ blankEs ++ reverse blks ++ reverse patternBlanks) in
    lexeme $ foldr1 (<|>) forms
-- ------------------------------------------------------------
-- slot
slot :: Parser Expr
slot = do
  char '#'
  return (Slot 1)

slotn :: Parser Expr
slotn = do
  char '#'
  n <- natural
  return (Slot (fromIntegral n))

slotSeq :: Parser Expr
slotSeq = do
  string "##"
  return (SlotSeq 1)

slotSeqn :: Parser Expr
slotSeqn = do
  string "##"
  n <- natural
  return (SlotSeq (fromIntegral n))

slots =
  let lis = [slotSeqn, slotSeq, slotn, slot] in
    lexeme $ foldr1 (<|>) (map try lis)
-----------------------------------
-- % Out
out :: Parser Expr
out = do
  lis <- many1 (char '%')
  return (Out (negate $ length lis))

outN :: Parser Expr
outN = do
  char '%'
  n <- natural
  return (Out (fromIntegral n))

outTerm = lexeme (try outN <|> try out)



-- ----------------------------------

expr :: Parser Expr
expr =
      opExpr
      <|> term

term :: Parser Expr
term = specialForms
      <|> slots
      <|> outTerm
      <|> var
      <|> number
      <|> stringE
      <|> charE
      <|> try partArgs
      <|> argument
      <|> list
      <|> parens expr

type Stage1 = Either ParseError Expr

-- data SemiExpr = Semi Expr | Nosemi Expr
--
-- fromSemi :: SemiExpr -> Expr
-- fromSemi (Semi e) = e
-- fromSemi (Nosemi e) = e

-- semiExpr :: Parser SemiExpr
-- semiExpr = do
--   ex <- expr
--   hasSemi <- (semi *> return Semi) <|> return Nosemi
--   return $ hasSemi ex

-- compoundExpr :: Parser Expr
-- compoundExpr = do
--   semiexs <- many1 semiExpr
--   let exs = map fromSemi semiexs
--   return $ case semiexs of
--     [Nosemi e] -> e
--     _ -> case last semiexs of
--       (Semi _) -> Compound (exs ++ [None])
--       (Nosemi _) -> Compound exs

parseExpr = parse (whiteSpace *> expr <* eof) "pass 1"
