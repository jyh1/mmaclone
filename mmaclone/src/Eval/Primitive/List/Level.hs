module Eval.Primitive.List.Level
  (-- * Module for level specification related functions
  levelFromTo, unpackLevelSpeci,unpackNormalLevelSpeci)
    where
import Data.DataType
import Data.Number.Number

import Control.Monad.Identity
import Control.Monad.Except

type LevelSpeci = (LispVal -> LispVal) -> LispVal -> LispVal

-- | Take a function and apply it to a lispval in the level range
-- specified by the pair of ints.
-- monad context to make general enough to implement Level like function.
-- Negative specification is not currently implemented
levelFromTo :: (Monad m) =>
  (LispVal -> m LispVal) ->
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


unpack :: LispVal -> LispVal -> IOThrowsError Int
unpack val = unpackInt (Level val)

-- | This function is used to unpack a level specification and
-- returns a function that will apply a supplied function to
-- a LispVal at desired level(s)
unpackLevelSpeci :: (Monad m) =>
  Int -- ^ Default level (will be used when the second argument is empty).
  -> [LispVal] -- ^ Level specification to be unpacked, could be empty, or taken as n, {n}, {i, j}.
  -> IOThrowsError ((LispVal -> m LispVal) -> LispVal -> m LispVal)
unpackLevelSpeci def [] = return $ \f -> levelAt f def
unpackLevelSpeci _ [val@(List [Atom "List",n])] = do
  n' <- unpack val n
  return $ \f -> levelAt f n'
unpackLevelSpeci _ [val@(List [Atom "List", i, j])] = do
  let unpack' = unpack val
  i' <- unpack' i
  j' <- unpack' j
  return $ \f -> levelFromTo f i' j'
unpackLevelSpeci _ [n] = do
  n' <- unpack n n
  return $ \f -> levelUpTo f n'
unpackLevelSpeci _ val = throwError $ Level (List val)

-- | This function will specified the Monad context in the
-- unpackLevelSpeci function to be Identity in order to free from
-- the Monad context. Used when defining Map, Apply ...
unpackNormalLevelSpeci :: Int -> [LispVal] -> IOThrowsError LevelSpeci
unpackNormalLevelSpeci n val = do
  levelSpeci <- unpackLevelSpeci n val
  return $ \f x -> runIdentity $ levelSpeci (Identity . f) x
