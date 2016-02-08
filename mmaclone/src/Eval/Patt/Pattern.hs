module Eval.Patt.Pattern where

import Data.DataType
import Data.Maybe

getMatch :: Pattern -> LispVal -> Maybe [Matched]
getMatch (List [Atom "Blank"]) _ = Just []
getMatch (List [Atom "Blank", Atom x]) (List (Atom y : _)) =
  fromBool $ x == y
getMatch (List [Atom "Pattern", Atom name ,pattern]) expr =
  fmap ((name, expr): ) $ getMatch pattern expr
getMatch (Atom a) (Atom b) = fromBool $ a == b
getMatch (Number a) (Number b) = fromBool $ a == b
getMatch (String a) (String b) = fromBool $ a == b
getMatch (Char a) (Char b) = fromBool $ a == b
getMatch (List a) (List b) =
  let sameL = length a == length b
      every = sequenceA $ zipWith getMatch a b in
    if sameL then fmap concat every else Nothing
getMatch _ _ = Nothing

fromBool :: Bool -> Maybe [Matched]
fromBool True = Just []
fromBool _ = Nothing

internalReplace :: LispVal -> [Matched]  -> LispVal
internalReplace val@(Atom name) ms =
  fromMaybe val (lookup name ms)
internalReplace (List ls) ms =
  List $ map (`internalReplace` ms) ls
internalReplace other _ = other

replace :: LispVal -> Rule -> Maybe LispVal
replace val (patt, target) = do
  matched <- getMatch patt val
  return $ internalReplace target matched
