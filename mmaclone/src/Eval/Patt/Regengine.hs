module Eval.Patt.Regengine where

import Data.DataType
import Data.Environment.EnvironmentType
import Eval.Primitive.PrimiFunc
import Eval.Patt.Pattern
import Data.Number.Number


import Control.Monad
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T

-- MatchState facility

type ParsedRule = (ParsedPatt, LispVal)

fromRule :: Rule -> ParsedRule
fromRule (p, l) = (transformLispPattern p, l)

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

-- try running a match program, if failed, revert environment and return False else return True
tryMatching :: MatchState a -> MatchState Bool
tryMatching (MatchState f) =
  let foo res = do
        run <- f res
        return $ Just $ case run of
          Nothing -> (res, False)
          Just (res', _) -> (res', True)
  in
    MatchState foo

runMatching :: MatchState () -> MatchResult
runMatching (MatchState f) = (fmap . fmap) fst (f initialMatch)


-- ------------------------------------------------------------
-- | replaceall a value with a set of match results
internalReplace :: LispVal -> MatchRes  -> LispVal
internalReplace val@(Atom name) ms =
  fromMaybe val (M.lookup name ms)
internalReplace (List ls) ms =
  List $ map (`internalReplace` ms) ls
internalReplace other _ = other

-- | convert bool to match result
fromBool :: Bool -> MatchState ()
fromBool True = emptyMatch
fromBool _ = matchFailed

-- regexp datatype ----------------------------

data AtomPatt = Literal LispVal
  | Blank
  | BlankSeq
  | BlankNullSeq
  deriving(Show)

type PattTest = LispVal -> MatchState ()

data ParsedPatt = Single AtomPatt
  | WithTest PattTest ParsedPatt
  | Alt [ParsedPatt]
  | Bind T.Text ParsedPatt
  | Then [ParsedPatt] [PatternType]


data PatternType = One
  | Seq
  | NullSeq

parsePatternType :: ParsedPatt -> PatternType
parsePatternType (Single BlankSeq) = Seq
parsePatternType (Single BlankNullSeq) = NullSeq
parsePatternType (Single _) = One
parsePatternType (WithTest _ p) = parsePatternType p
parsePatternType (Alt ps) = pattSum (map parsePatternType ps)
  where
    pattSum = foldr f One
    f One x = x
    f Seq One = Seq
    f Seq NullSeq = NullSeq
    f NullSeq _ = NullSeq
parsePatternType (Bind _ p) = parsePatternType p
parsePatternType (Then ps _) = One


makePatternTest :: LispVal -> PattTest
makePatternTest f b = patternTest (applyHead f b)

makeCondition :: LispVal -> PattTest
makeCondition f _ = do
  match <- getMatchRes
  patternTest (internalReplace f match)

makeBlankTest :: T.Text -> PattTest
makeBlankTest name (List (Atom matched :_)) =
  fromBool (name == matched)
makeBlankTest "Integer" (Number (Integer _)) = emptyMatch
makeBlankTest "Rational" (Number (Rational _)) = emptyMatch
makeBlankTest "Real" (Number (Double _)) = emptyMatch
makeBlankTest "Symbol" (Atom _) = emptyMatch
makeBlankTest _ _ = matchFailed

mapSequence :: PattTest -> PattTest
mapSequence k (List ((Atom "Sequence") : ls)) =
  mapM_ k ls
mapSequence _ _ = matchFailed


-- parse reg expr from LispVal
type ParseLispval = LispVal -> Maybe ParsedPatt

parseBlank :: ParseLispval
parseBlank (List [Atom "Blank"]) =
  Just (Single Blank)
parseBlank (List [Atom "Blank", Atom y]) =
  Just (WithTest (makeBlankTest y) (Single Blank))
parseBlank _ = Nothing


blanks = ["BlankSequence", "BlankNullSequence"]
blankCons = zip blanks [BlankSeq, BlankNullSeq]

parseBlankSeq :: ParseLispval
parseBlankSeq (List [Atom x]) =
  fmap Single $ lookup x blankCons
parseBlankSeq (List [Atom x, Atom y]) =
  fmap (WithTest (mapSequence (makeBlankTest y)) . Single) $ lookup x blankCons
parseBlankSeq _ = Nothing

parsePattern :: ParseLispval
parsePattern (List [Atom "Pattern", Atom name, pattern]) =
  fmap (Bind name) $ parsePatt pattern
parsePattern _ = Nothing

patternTestType :: ParsedPatt -> PattTest -> PattTest
patternTestType pp f =
  case parsePatternType pp of
    One -> f
    _ -> mapSequence f

parsePatternTest :: ParseLispval
parsePatternTest (List [Atom "PatternTest", p, f]) =
  do
    parsed <- parsePatt p
    return (WithTest (patternTestType parsed (makePatternTest f)) parsed)
parsePatternTest _ = Nothing

parseCondition :: ParseLispval
parseCondition (List [Atom "Condition", p, f]) =
  fmap (WithTest (makeCondition f)) $ parsePatt p
parseCondition _ = Nothing

parseAlternative :: ParseLispval
parseAlternative (List (Atom "Alternatives":as)) = do
  rest <- mapM parsePatt as
  return (Alt rest)
parseAlternative _ = Nothing

liftLiteral :: LispVal -> ParsedPatt
liftLiteral = Single . Literal

parseList :: ParseLispval
parseList (List ls) = do
  rest <- mapM parsePatt ls
  return (Then rest (map parsePatternType rest))
parseList _ = Nothing

parseLiteral :: ParseLispval
parseLiteral x = Just (liftLiteral x)

parsers = [parseBlank, parseBlankSeq, parsePattern, parsePatternTest,
  parseCondition, parseAlternative, parseList, parseLiteral]

parsePatt :: ParseLispval
parsePatt val = msum (map ($ val) parsers)

transformLispPattern :: LispVal -> ParsedPatt
transformLispPattern = fromJust . parsePatt

-- ----------------------------------------------------

patternMatching :: ParsedPatt -> LispVal -> MatchState ()
patternMatching (Single (Literal p)) l = fromBool $ p == l
patternMatching (Single _) l = emptyMatch
patternMatching (WithTest test p) l =
  let
    allP = allPossibleMatch p l
  in
    tryMatchList (map (>> test l) allP)
patternMatching (Alt alts) l = matchAlt alts l
patternMatching (Bind name p) l = do
  patternMatching p l
  addNewMatch name l
patternMatching (Then ps ts) (List ls) =
  matchThen ps ts ls
patternMatching _ _ = matchFailed

allPossibleMatch :: ParsedPatt -> LispVal -> [MatchState ()]
allPossibleMatch (Then ps ts) (List ls) = matchThenAll ps ts ls
allPossibleMatch patt lisp = [patternMatching patt lisp]

tryMatchList :: [MatchState ()] -> MatchState ()
tryMatchList [] = matchFailed
tryMatchList (m:ms) = do
  flag <- tryMatching m
  if flag then
    emptyMatch
  else tryMatchList ms

matchAlt :: [ParsedPatt] -> LispVal -> MatchState ()
matchAlt ps l = tryMatchList (map (`patternMatching` l) ps)


splitsFrom s st = [splitAt n st | n <- [s .. length st]]
-- splits = splitsFrom 0
-- frontSplit = splitsFrom 1

matchThenAll :: [ParsedPatt] -> [PatternType] -> [LispVal] -> [MatchState ()]
matchThenAll [] _ [] = [emptyMatch]
matchThenAll [] _ _ = []
matchThenAll [p] [NullSeq] ls = [patternMatching p (wrapSequence ls)]
matchThenAll [p] [Seq] ls
  | ls == [] = []
  | otherwise = [patternMatching p (wrapSequence ls)]
matchThenAll (p:ps) (t:ts) ls =
  let
    allocateMatch n =
      -- [patternMatching p (wrapSequence fs) >> rest
      [rest >> patternMatching p (wrapSequence fs)
        | (fs, bs) <- splitsFrom n ls, rest <- matchThenAll ps ts bs]
  in
    case t of
      One ->
        if (ls == []) then []
          else
            map (patternMatching p (head ls) >>) (matchThenAll ps ts (tail ls))
      Seq ->
        allocateMatch 1
      NullSeq -> allocateMatch 0



matchThen :: [ParsedPatt] -> [PatternType] -> [LispVal] -> MatchState ()
matchThen p t l = tryMatchList (matchThenAll p t l)
