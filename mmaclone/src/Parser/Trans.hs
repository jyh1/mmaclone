{-#LANGUAGE ExistentialQuantification #-}

module Parser.Trans(transform,negateE,inverseE,readExpr) where

import Data.DataType hiding (addHead)
import Data.Number.Number
import Parser.NewParse
import Control.Monad.Except
import Control.Monad(msum)
import qualified Data.Text as T

readExpr :: String -> ThrowsError LispVal
readExpr = transform . parseExpr

transform :: Stage1 -> ThrowsError LispVal
transform (Left err) = throwError $ Parser err
transform (Right expr) = expr2LispVal expr

expr2LispVal :: Expr -> ThrowsError LispVal
expr2LispVal (Args args) = do
  tran <- mapM expr2LispVal args
  throwError $ Incomplete tran

expr2LispVal (PartArgs args) = do
  tran <- mapM expr2LispVal args
  throwError $ Incomplete tran

expr2LispVal (Lis lis) = do
  tran <- mapM expr2LispVal lis
  return $ addHead "List" tran

expr2LispVal (Add val@(Add _ _) e3) =
  flatten val e3
expr2LispVal (Add e1 e2) =
  twoArgs (addHead2 "Plus") e1 e2

expr2LispVal (Mul val@(Mul _ _) e) =
  flatten val e
expr2LispVal (Mul e1 e2) =
  twoArgs (addHead2 "Times") e1 e2

expr2LispVal (Pow e1 e2) =
  twoArgs (addHead2 "Power") e1 e2

expr2LispVal (And val@(And _ _) e) =
  flatten val e
expr2LispVal (And e1 e2) =
  twoArgs (addHead2 "And") e1 e2

expr2LispVal (Or val@(Or _ _) e) =
  flatten val e
expr2LispVal (Or e1 e2) =
  twoArgs (addHead2 "Or") e1 e2

expr2LispVal (Not e) =
  oneArg (addHead1 "Not") e

expr2LispVal (Equal e1 e2) = equalTrans "Equal" e1 e2
expr2LispVal (Less e1 e2) = equalTrans "Less" e1 e2
expr2LispVal (LessEq e1 e2) = equalTrans "LessEqual" e1 e2
expr2LispVal (Great e1 e2) = equalTrans "Greater" e1 e2
expr2LispVal (GreatEq e1 e2) = equalTrans "GreaterEqual" e1 e2
expr2LispVal (UnEq e1 e2) = equalTrans "Unequal" e1 e2

expr2LispVal (Compound val@(Compound _ _) e) =
  flatten val e
expr2LispVal (Compound e1 e2) =
  twoArgs (addHead2 "CompoundExpression") e1 e2

expr2LispVal (Apply h (Args args)) =
  listArgs apply h args

expr2LispVal (Fact e) =
  oneArg (addHead1 "Factorial") e
expr2LispVal (Fact2 e) =
  oneArg (addHead1 "Factorial2") e

expr2LispVal (Part h (PartArgs args)) =
  listArgs apply' h args
    where apply' e es = List $ Atom "Part":e:es

expr2LispVal (Map e1 e2) =
  twoArgs (addHead2 "Map") e1 e2

expr2LispVal (MapAll e1 e2) =
  twoArgs (addHead2 "MapAll") e1 e2

expr2LispVal (Apply1 e1 e2) =
  twoArgs (addHead2 "Apply") e1 e2

expr2LispVal (Apply11 e1 e2) =
  twoArgs apply' e1 e2
    where apply' l1 l2 = List [Atom "Apply",l1,l2,list [integer 1]]

expr2LispVal (Derivative n e) =
  let
    n' = integer n
    deriv l = List [List [Atom "Derivative",n'],l] in
  oneArg deriv e

expr2LispVal (Rule e1 e2) =
  twoArgs (addHead2 "Rule") e1 e2
expr2LispVal (RuleDelayed e1 e2) =
  twoArgs (addHead2 "RuleDelayed") e1 e2

expr2LispVal (Replace e1 e2) =
  twoArgs (addHead2 "ReplaceAll") e1 e2
expr2LispVal (ReplaceRepeated e1 e2) =
  twoArgs (addHead2 "ReplaceRepeated") e1 e2

expr2LispVal (Set e1 e2) =
  twoArgs (addHead2 "Set") e1 e2
expr2LispVal (SetDelayed e1 e2) =
  twoArgs (addHead2 "SetDelayed") e1 e2

expr2LispVal (Unset e) =
  oneArg (addHead1 "Unset") e

expr2LispVal (Dot e1 e2) =
  twoArgs (addHead2 "Dot") e1 e2

expr2LispVal Blk =
  return $ List [Atom "Blank"]
expr2LispVal (BlkE e) =
  oneArg (addHead1 "Blank") e

expr2LispVal BlkSeq =
  return $ List [Atom "BlankSequence"]
expr2LispVal (BlkSeqE e) =
  oneArg (addHead1 "BlankSequence") e

expr2LispVal NullSeq =
  return $ List [Atom "BlankNullSequence"]
expr2LispVal (NullSeqE e) =
  oneArg (addHead1 "BlankNullSequence") e

expr2LispVal (Pattern e1 e2) =
  twoArgs (addHead2 "Pattern") e1 e2

expr2LispVal (PatternTest e1 e2) =
  twoArgs (addHead2 "PatternTest") e1 e2

expr2LispVal (Function e) =
  oneArg (addHead1 "Function") e

expr2LispVal (Cond e1 e2) =
  twoArgs (addHead2 "Condition") e1 e2

expr2LispVal (Alter val@(Alter _ _) e) =
  flatten val e
expr2LispVal (Alter e1 e2) =
  twoArgs (addHead2 "Alternatives") e1 e2

expr2LispVal (Negate e) = do
  e' <- expr2LispVal e
  return $ case e' of
    Number n -> Number (- n)
    other -> negateE other

expr2LispVal (Inverse e) = do
  e' <- expr2LispVal e
  return $ inverseE e'



expr2LispVal other = return $ trivial other

trivial :: Expr -> LispVal
trivial (Num num) = Number num
trivial (Var name) = Atom name
trivial None = atomNull
trivial (Slot n) = addHead1 "Slot" (integer n)
trivial (SlotSeq n) = addHead1 "SlotSequence" (integer n)
trivial (Str s) = String s
trivial (Chr c) = Char c
trivial (Out n) = addHead1 "Out" (integer n)
-- trivial (Lis lis) = (Atom "List") : lis

negateE :: LispVal -> LispVal
negateE e = List [Atom "Times", integer (-1), e]

inverseE :: LispVal -> LispVal
inverseE n = List [Atom "Power", n, integer (-1)]

-- equal unpacker -----------------------
unPackEqual (Equal e1 e2) = Just (Equal e1 e2)
unPackEqual _ = Nothing
unPackLess (Less e1 e2) = Just (Less e1 e2)
unPackLess _ = Nothing
unPackLessEq (LessEq e1 e2) = Just (LessEq e1 e2)
unPackLessEq _ = Nothing
unPackGreat (Great e1 e2) = Just (Great e1 e2)
unPackGreat _ =Nothing
unPackGreatEq (GreatEq e1 e2) = Just (GreatEq e1 e2)
unPackGreatEq _ = Nothing
unPackUnEq (UnEq e1 e2) = Just (UnEq e1 e2)
unPackUnEq _ = Nothing

eqUnpackers = [unPackEqual, unPackLess,unPackLessEq,
              unPackGreat,unPackGreatEq,unPackUnEq]

getEqTrans :: Expr -> Maybe (ThrowsError LispVal)
getEqTrans e = do
  expr <- msum $ map ($ e) eqUnpackers
  return $ expr2LispVal expr

equalTrans :: T.Text -> Expr -> Expr -> ThrowsError LispVal
equalTrans name e1 e2 = do
  let trans = getEqTrans e1
  case trans of
    Nothing -> do
      e1' <- expr2LispVal e1
      e2' <- expr2LispVal e2
      return $ List [Atom "Inequality",e1',Atom name,e2']
    (Just e) -> do
      tranedE1 <- e
      e2' <- expr2LispVal e2
      return $ case tranedE1 of
        (List lis) -> List (lis ++ [Atom name,e2'])

-- args transform
addHead :: T.Text -> [LispVal] -> LispVal
addHead na vs = List $ Atom na : vs


addHead1 :: T.Text -> LispVal -> LispVal
addHead1 atom v = List [Atom atom,v]

addHead2 :: T.Text -> LispVal -> LispVal -> LispVal
addHead2 atom v1 v2 = List [Atom atom,v1,v2]

patternBlk h l = addHead1 "Pattern" (addHead1 h l)


oneArg :: (LispVal -> LispVal) -> Expr -> ThrowsError LispVal
oneArg f e = do
  e' <- expr2LispVal e
  return $ f e'


twoArgs :: (LispVal -> LispVal -> LispVal) -> Expr -> Expr
             -> ThrowsError LispVal
twoArgs f e1 e2 = do
  e1' <- expr2LispVal e1
  e2' <- expr2LispVal e2
  return $ f e1' e2'

listArgs :: (LispVal -> [LispVal] -> LispVal) -> Expr -> [Expr]
              -> ThrowsError LispVal
listArgs f e es = do
  e' <- expr2LispVal e
  es' <- mapM expr2LispVal es
  return $ f e' es'



apply :: LispVal -> [LispVal] -> LispVal
apply = (List.) . (:)
-----------------------------------------------

-- flatten transform
flatten :: Expr -> Expr -> ThrowsError LispVal
flatten val e3 = do
  trans <- expr2LispVal val
  e3' <- expr2LispVal e3
  return $ case trans of
    List lis -> List (lis ++ [e3'])
