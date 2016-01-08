module DataType where

import Control.Monad.Except
import           Text.ParserCombinators.Parsec(ParseError)


import Data.Ratio
import Hier

-- Number Type
data Number = Double Double
            | Rational Rational
            | Integer Integer
  deriving(Eq,Ord)

instance Show Number where
  show (Integer i) = show i
  show (Double d) = show d
  show (Rational r) = show r

instance Hier Number where
  rank (Integer _) = 1
  rank (Rational _) = 2
  rank (Double _) = 3

  upgrade (Integer i) = Rational $ i % 1
  upgrade (Rational r) = Double $ fromRational r

  downgrade (Double d) = Rational $ toRational d
  downgrade (Rational r) = Integer (truncate $ fromRational r)


plus :: Number -> Number -> Number
plus = peerOpUp plus'
  where plus' (Integer i1) (Integer i2) = Integer $ i1 + i2
        plus' (Rational r1) (Rational r2) = Rational (r1 + r2)
        plus' (Double d1) (Double d2) = Double $ d1 + d2

minus :: Number -> Number -> Number
minus = peerOpUp minus'
  where minus' (Integer i1) (Integer i2) = Integer $ i1 - i2
        minus' (Rational r1) (Rational r2) = Rational $ r1 - r2
        minus' (Double d1) (Double d2) = Double $ d1 - d2

times :: Number -> Number -> Number
times = peerOpUp times'
  where times' (Integer i1) (Integer i2) = Integer $ i1 * i2
        times' (Rational r1) (Rational r2) = Rational $ r1 * r2
        times' (Double d1) (Double d2) = Double $ d1 * d2

divide :: Number -> Number -> Number
divide = peerOpUp divide'
  where divide' (Integer i1) (Integer i2)
          | i1 `mod` i2 == 0= Integer $ i1 `div` i2
          | otherwise = Rational $ i1 % i2
        divide' (Rational r1) (Rational r2) = Rational $ r1 / r2
        divide' (Double d1) (Double d2) = Double $ d1 / d2

modN :: Number -> Number -> Number
modN = peerOpUp modN'
  where modN' (Integer i1) (Integer i2) = Integer $ i1 `mod` i2
        modN' (Double d1) (Double d2) = Double (d1 - (fromIntegral . truncate) (d1 / d2) * d2)

-- quoteientN :: Number -> Number -> Number
-- ---------------------------------------

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
  show None = undefined


-- ------------------------------------------

-- LispError

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

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- --------------------------------------------------
