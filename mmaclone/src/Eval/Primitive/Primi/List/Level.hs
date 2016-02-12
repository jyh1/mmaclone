module Eval.Primitive.Primi.List.Level
  (levelMap,levelFromTo,levelUpTo,levelAt,levelMapFromTo,levelMapUpTo) where
import Data.DataType
import Data.Number.Number

import Control.Monad.Identity
-- import Eval.Primitive.PrimiType

-- levelMap :: (LispVal -> LispVal) -> Int -> LispVal -> LispVal
-- levelMap f 0 x = f x
-- levelMap f n (List (l:ls)) =
--   let newf = levelMap f (n - 1) in
--     List (l : map newf ls)
-- levelmap _ _ other = other
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

levelMap f n = runIdentity . levelAt (Identity . f) n

levelMapFromTo f i j = runIdentity . levelFromTo (Identity . f) i j

levelMapUpTo f n = runIdentity . levelUpTo (Identity . f) n
