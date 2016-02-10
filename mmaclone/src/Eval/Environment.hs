module Eval.Environment(evalWithEnv) where
import Data.DataType
import Data.Environment.Environment

import Data.IORef
import Control.Monad

evalWithEnv :: Env -> LispVal -> IOThrowsError LispVal
evalWithEnv env lhs
  | validSet lhs = liftM (replaceContext lhs) (readCont env)
  | otherwise = return lhs

-- contextReplace :: LispVal -> Context -> LispVal
-- contextReplace val cont =
--   let valueEval = valueMatching val (value cont)
--       patternEval = patternMatching val (pattern cont)
--       total = mplus valueEval patternEval in
--     fromMaybe val total
--
--
-- patternMatching :: LispVal -> PatternRule -> Maybe LispVal
-- patternMatching val@(List (Atom name:_)) pattRule =
--   let find v = msum . map (replace v) in
--     M.lookup name pattRule >>= find val
-- patternMatching _ _ = Nothing
--
-- valueMatching :: LispVal -> ValueRule -> Maybe LispVal
-- valueMatching = M.lookup
