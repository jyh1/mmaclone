module Eval.Primitive.Primi.List.Level
  -- (levelMap,levelFromTo,levelUpTo,levelAt,levelMapFromTo,levelMapUpTo)
  (unpackLevelSpeci,unpackNormalLevelSpeci)
    where
import Data.DataType
import Data.Number.Number

import Control.Monad.Identity
import Control.Monad.Except

type LevelSpeci = (LispVal -> LispVal) -> LispVal -> LispVal

levelFromTo :: (Monad m) => (LispVal -> m LispVal) ->
  Int -> Int -> LispVal -> m LispVal
levelFromTo f 0 0 x = f x
levelFromTo f 0 j (List (l:ls)) = do
  let newf = levelFromTo f 0 (j - 1)
  mapped <- mapM newf ls
  f $ List (l : mapped)
levelFromTo f 0 j other = f other
levelFromTo f i j  (List (l : ls)) = do
  let newf = levelFromTo f (i - 1) (j - 1)
  mapped <- mapM newf ls
  return $ List (l : mapped)
levelFromTo _ _ _ other = return other

levelAt f n = levelFromTo f n n

levelUpTo f = levelFromTo f 1

-- levelMap f n = runIdentity . levelAt (Identity . f) n
--
-- levelMapFromTo f i j = runIdentity . levelFromTo (Identity . f) i j
--
-- levelMapUpTo f n = runIdentity . levelUpTo (Identity . f) n

unpack :: LispVal -> LispVal -> ThrowsError Int
unpack _ (Number (Integer n)) = return (fromIntegral n)
unpack val _ = throwError $ Level val


unpackLevelSpeci :: (Monad m) =>
  Int -> [LispVal] -> ThrowsError
    ((LispVal -> m LispVal) -> LispVal -> m LispVal)
unpackLevelSpeci def [] = return $ \f -> levelAt f def
unpackLevelSpeci _ [val@(List [Atom "List",n])] = do
  n' <- unpack val n
  return $ \f -> levelAt f n'
unpackLevelSpeci _ [val@(List [Atom "List", i,j])] = do
  let unpack' = unpack val
  i' <- unpack' i
  j' <- unpack' j
  return $ \f -> levelFromTo f i' j'
unpackLevelSpeci _ [n] = do
  n' <- unpack n n
  return $ \f -> levelUpTo f n'
-- unpackLevelSpeci _ val = throwError $ Level (List val)


unpackNormalLevelSpeci :: Int -> [LispVal] -> ThrowsError LevelSpeci
unpackNormalLevelSpeci n val = do
  levelSpeci <- unpackLevelSpeci n val
  return $ \f x -> runIdentity $ levelSpeci (Identity . f) x
