module Eval.Patt.Pattern where

import Data.DataType


import Data.Maybe
import Data.Function(on)
import Control.Monad
import qualified Data.Text as T

type Pattern = LispVal
type Matched = (T.Text, LispVal)
type Rule = (Pattern, LispVal)

-- getMatch :: Pattern -> LispVal -> Maybe [Matched]
-- getMatch (List [Atom "Blank"]) _ = Just []
-- getMatch (List [Atom "Blank", Atom x]) (List (Atom y : _)) =
--   fromBool $ x == y
-- getMatch (List [Atom "Pattern", Atom name ,pattern]) expr =
--   fmap ((name, expr): ) $ getMatch pattern expr
-- getMatch (Atom a) (Atom b) = fromBool $ a == b
-- getMatch (Number a) (Number b) = fromBool $ a == b
-- getMatch (String a) (String b) = fromBool $ a == b
-- getMatch (Char a) (Char b) = fromBool $ a == b
-- getMatch (List a) (List b) =
--   let sameL = length a == length b
--       every = sequenceA $ zipWith getMatch a b in
--     if sameL then fmap concat every else Nothing
-- getMatch _ _ = Nothing
--
-- fromBool :: Bool -> Maybe [Matched]
-- fromBool True = Just []
-- fromBool _ = Nothing
--
-- internalReplace :: LispVal -> [Matched]  -> LispVal
-- internalReplace val@(Atom name) ms =
--   fromMaybe val (lookup name ms)
-- internalReplace (List ls) ms =
--   List $ map (`internalReplace` ms) ls
-- internalReplace other _ = other
--
-- replace :: LispVal -> Rule -> Maybe LispVal
-- replace val (patt, target) = do
--   matched <- getMatch patt val
--   return $ internalReplace target matched
--
-- replaceRuleList :: LispVal -> [Rule] -> Maybe LispVal
-- replaceRuleList val rules =
--   msum (map (replace val) rules)
--
-- tryReplaceRuleList :: LispVal -> [Rule] -> LispVal
-- tryReplaceRuleList val = fromMaybe val . replaceRuleList val
--
-- replaceAll :: [Rule] -> LispVal -> LispVal
-- replaceAll rules val =
--   let ifFailed =
--         case val of
--           List lis -> List (map (replaceAll rules) lis)
--           _ -> val in
--     fromMaybe ifFailed (replaceRuleList val rules)

blankQ :: Pattern -> Bool
blankQ (List (Atom "Blank" : _)) = True
blankQ _ = False

blankEq :: Pattern -> Pattern -> Bool
blankEq a b
  | blankQ a && blankQ b = True
  | otherwise = False

blankEqui = blankEq `on` unpackPatt

unpackPatt :: Pattern -> Pattern
unpackPatt (List [Atom "Pattern",_,patt]) = patt
unpackPatt other = other

patternEqui :: Pattern -> Pattern -> Bool
patternEqui (List as) (List bs) =
  let el = length as == length bs
      pl = and $ zipWith patternEqui as bs in
    el && pl
patternEqui a b = a == b || blankEqui a b


isPattern :: LispVal -> Bool
isPattern (List (Atom "Pattern" : _)) = True
isPattern (List (Atom "Blank":_)) = True
isPattern (List xs) = any isPattern xs
isPattern _ = False
