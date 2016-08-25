module Eval.Patt.PatternPrimi where

import Data.DataType
import Data.Environment.EnvironmentType
import Eval.Primitive.PrimiFunc

import qualified Data.Text as T
import Control.Monad
import Data.Maybe



getMatch :: Pattern -> LispVal -> MatchResult
getMatch (List [Atom "Blank"]) _ = return (Just [])
getMatch (List [Atom "Blank", Atom x]) (List (Atom y : _)) =
  fromBool $ x == y
getMatch (List [Atom "Pattern", Atom name, pattern]) expr =
  (fmap . fmap) ((name, expr): ) $ getMatch pattern expr
getMatch (List [Atom "PatternTest", p, f]) b = do
  let checkTest = do
        test <- evaluate (applyHead f b)
        return $ if (trueQ test) then Just [] else Nothing
  matchAnd (getMatch p b) checkTest
getMatch (Number a) (Number b) = fromBool $ a == b
getMatch (String a) (String b) = fromBool $ a == b
getMatch (Char a) (Char b) = fromBool $ a == b
getMatch (List a) (List b) =
  let sameL = length a == length b
      every = getMatchList a b in
    if sameL then every else return Nothing
getMatch _ _ = return Nothing

-- | mannualy write foldM
getMatchList :: [Pattern] -> [LispVal] -> MatchResult
getMatchList [] _ = return (Just [])
getMatchList (p:ps) (l:ls) =
  matchAnd (getMatch p l) (getMatchList ps ls)


checkMatch :: MatchResult -> MaybeMatch -> MatchResult
checkMatch res (Just val) = do
  res' <- res
  return $ case res' of
    Nothing -> Nothing
    (Just mat) -> Just (val ++ mat)
checkMatch _ Nothing = return Nothing

matchAnd :: MatchResult -> MatchResult -> MatchResult
matchAnd a1 a2 = do
  res1 <- a1
  checkMatch a2 res1

-- | convert bool to match result
fromBool :: Bool -> MatchResult
fromBool True = return $ Just []
fromBool _ = return Nothing

-- | replaceall a value with a set of match results
internalReplace :: LispVal -> [Matched]  -> LispVal
internalReplace val@(Atom name) ms =
  fromMaybe val (lookup name ms)
internalReplace (List ls) ms =
  List $ map (`internalReplace` ms) ls
internalReplace other _ = other

-- | replace a lispval with a pattern matching specification
replace :: LispVal -> Rule -> ReplaceResult
replace val (patt, target) = do
  matched <- getMatch patt val
  return $ fmap (internalReplace target) matched

-- | replace at the top level with a list of rule, return the first success, lazy state assures short circuit
replaceRuleList :: LispVal -> [Rule] -> ReplaceResult
replaceRuleList val rules =
  liftM msum (mapM (replace val) rules)

tryReplaceRuleList :: LispVal -> [Rule] -> Primi
tryReplaceRuleList val = (liftM $ fromMaybe val) . replaceRuleList val

-- | replace all with a list of rule, top-down
replaceAll :: [Rule] -> LispVal -> Primi
replaceAll rules val =
  let ifFailed =
        case val of
          List lis -> (liftM List) (mapM (replaceAll rules) lis)
          _ -> return val in
    do
      now <- replaceRuleList val rules
      case now of
        Nothing -> ifFailed
        Just val -> return val
