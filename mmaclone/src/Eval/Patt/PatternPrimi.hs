module Eval.Patt.PatternPrimi where

import Data.DataType
import Data.Environment.EnvironmentType
import Eval.Primitive.PrimiFunc

import qualified Data.Text as T
import Control.Monad
import Data.Maybe
import qualified Data.Map.Strict as M


data MatchState a =
  MatchState {getMatchF :: MatchRes -> StateResult (Maybe (MatchRes, a))}

instance Functor MatchState where
  fmap f (MatchState patt) =
    let foo res = do
          matchRes <- patt res
          return $ case matchRes of
            Nothing -> Nothing
            Just (res', ans) -> Just (res', f ans)
    in
      MatchState foo

instance Applicative MatchState where
  pure a = MatchState (\res -> return (Just (res, a)))
  (MatchState f1) <*> m2 =
    let foo res = do
          matchRes <- f1 res
          case matchRes of
            Nothing -> return Nothing
            Just (res', ansF) -> getMatchF (fmap ansF m2) res'
    in
      MatchState foo

instance Monad MatchState where
  return = pure
  (MatchState f1) >>= f2 =
    let foo res = do
          matchRes <- f1 res
          case matchRes of
            Nothing -> return Nothing
            Just (res', r1) -> getMatchF (f2 r1) res'
    in
      MatchState foo


updateMatch :: (MatchRes -> MatchRes) -> MatchState ()
updateMatch f =
  let foo res =
        return (Just (f res, ())) in
    MatchState foo

getMatchRes :: MatchState MatchRes
getMatchRes =
  let foo res =
        return (Just (res, res)) in
    MatchState foo

addNewMatch :: T.Text -> LispVal -> MatchState ()
addNewMatch name expr = do
  res <- getMatchRes
  let checker = fmap (expr ==) (M.lookup name res)
  case checker of
    Nothing -> updateMatch (M.insert name expr)
    Just True -> return ()
    Just False -> matchFailed

matchFailed :: MatchState a
matchFailed = MatchState (const (return Nothing))

emptyMatch :: MatchState ()
emptyMatch = return ()

patternTest :: LispVal -> MatchState ()
patternTest cond =
  let foo res = do
        test <- evaluate cond
        return $ if (trueQ test) then Just (res, ()) else Nothing
  in
    MatchState foo

runMatching :: MatchState () -> MatchResult
runMatching (MatchState f) = (fmap . fmap) fst (f initialMatch)

patternMatching :: Pattern -> LispVal -> MatchState ()
patternMatching (List [Atom "Blank"]) _ = emptyMatch
patternMatching (List [Atom "Blank", Atom x]) (List (Atom y : _)) =
  fromBool $ x == y
patternMatching (List [Atom "Pattern", Atom name, pattern]) expr = do
  patternMatching pattern expr
  addNewMatch name expr
patternMatching (List [Atom "PatternTest", p, f]) b = do
  patternMatching p b
  patternTest (applyHead f b)
patternMatching (List [Atom "Condition", p, f]) b = do
  patternMatching p b
  match <- getMatchRes
  patternTest (internalReplace f match)
      
patternMatching (Atom a) (Atom b) = fromBool $ a == b
patternMatching (Number a) (Number b) = fromBool $ a == b
patternMatching (String a) (String b) = fromBool $ a == b
patternMatching (Char a) (Char b) = fromBool $ a == b
patternMatching (List a) (List b) =
  let sameL = length a == length b
      every = zipWithM_ patternMatching a b in
    if sameL then every else matchFailed
patternMatching _ _ = matchFailed

getMatch :: Pattern -> LispVal -> MatchResult
getMatch p l = runMatching (patternMatching p l)


-- | convert bool to match result
fromBool :: Bool -> MatchState ()
fromBool True = emptyMatch
fromBool _ = matchFailed

-- | replaceall a value with a set of match results
internalReplace :: LispVal -> MatchRes  -> LispVal
internalReplace val@(Atom name) ms =
  fromMaybe val (M.lookup name ms)
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
  fmap msum (mapM (replace val) rules)

tryReplaceRuleList :: LispVal -> [Rule] -> Primi
tryReplaceRuleList val = fmap (fromMaybe val) . replaceRuleList val

-- | replace all with a list of rule, top-down
replaceAll :: [Rule] -> LispVal -> Primi
replaceAll rules val =
  let ifFailed =
        case val of
          List lis -> fmap List (mapM (replaceAll rules) lis)
          _ -> return val in
    do
      now <- replaceRuleList val rules
      case now of
        Nothing -> ifFailed
        Just val -> return val
