{-#LANGUAGE ExistentialQuantification #-}

module Trans(transform,negateE,inverseE) where

import DataType
import Number
import NewParse
import Control.Monad.Except
import Control.Monad(msum)

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
  return $ List (Atom "List" : tran)

expr2LispVal (Add val@(Add _ _) e3) = do
  trans <- expr2LispVal val
  e3' <- expr2LispVal e3
  return $ case trans of
    List lis -> List (lis ++ [e3'])
expr2LispVal (Add e1 e2) = do
  e1' <- expr2LispVal e1
  e2' <- expr2LispVal e2
  return $ List [Atom "Plus", e1', e2']

expr2LispVal (Mul val@(Mul _ _) e) = do
  trans <- expr2LispVal val
  e' <- expr2LispVal e
  return $ case trans of
    List lis -> List (lis ++ [e'])
expr2LispVal (Mul e1 e2) = do
  e1' <- expr2LispVal e1
  e2' <- expr2LispVal e2
  return $ List [Atom "Times", e1', e2']

expr2LispVal (And val@(And _ _) e) = do
  trans <- expr2LispVal val
  e' <- expr2LispVal e
  return $ case trans of
    List lis -> List $ lis ++ [e']
expr2LispVal (And e1 e2) = do
  e1' <- expr2LispVal e1
  e2' <- expr2LispVal e2
  return $ List [Atom "And", e1', e2']

expr2LispVal (Or val@(Or _ _) e) = do
  trans <- expr2LispVal val
  e' <- expr2LispVal e
  return $ case trans of
    List lis -> List $ lis ++ [e']
expr2LispVal (Or e1 e2) = do
  e1' <- expr2LispVal e1
  e2' <- expr2LispVal e2
  return $ List [Atom "Or", e1', e2']

expr2LispVal (Not e) = do
  e' <- expr2LispVal e
  return $ List [Atom "Not", e']

expr2LispVal (Equal e1 e2) = equalTrans "Equal" e1 e2
expr2LispVal (Less e1 e2) = equalTrans "Less" e1 e2
expr2LispVal (LessEq e1 e2) = equalTrans "LessEqual" e1 e2
expr2LispVal (Great e1 e2) = equalTrans "Great" e1 e2
expr2LispVal (GreatEq e1 e2) = equalTrans "GreatEqual" e1 e2
expr2LispVal (UnEq e1 e2) = equalTrans "Unequal" e1 e2



expr2LispVal (Negate e) = do
  e' <- expr2LispVal e
  return $ case e' of
    Number n -> Number (negateN n)
    other -> negateE other

expr2LispVal (Inverse e) = do
  e' <- expr2LispVal e
  return $ inverseE e'



expr2LispVal other = return $ trivial other

trivial :: Expr -> LispVal
trivial (Num num) = Number num
trivial (Var name) = Atom name
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

equalTrans :: String -> Expr -> Expr -> ThrowsError LispVal
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
