module Environment(evalWithEnv) where
import DataType
-- import Hier
import Pattern
-- import Attribute

import Control.Monad
-- import Control.Monad.Except
import Data.Maybe(fromMaybe)
-- import Data.List(partition, genericLength, genericIndex)
import qualified Data.Map.Strict as M

evalWithEnv :: Env -> LispVal -> IOThrowsError LispVal
evalWithEnv env lhs =
  liftM (contextReplace lhs) (readRule env)

contextReplace :: LispVal -> Context -> LispVal
contextReplace val cont =
  let valueEval = valueMatching val (value cont)
      patternEval = patternMatching val (pattern cont)
      total = mplus valueEval patternEval in
    fromMaybe val total


patternMatching :: LispVal -> PatternRule -> Maybe LispVal
patternMatching val@(List (Atom name:_)) pattRule =
  let find v = msum . map (replace v) in
    M.lookup name pattRule >>= find val
patternMatching _ _ = Nothing

valueMatching :: LispVal -> ValueRule -> Maybe LispVal
valueMatching = M.lookup
