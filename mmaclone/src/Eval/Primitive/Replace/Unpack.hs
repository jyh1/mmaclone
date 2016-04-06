
module Eval.Primitive.Primi.Replace.Unpack
  (unpackReplaceArg) where

import Data.DataType
import Eval.Patt.Pattern

import Control.Monad.Except

reps val = Default (show val ++ " cannot be used for replacing.")

unpack :: LispVal -> Maybe Rule
unpack (List [Atom "Rule",a,b]) = Just (a,b)
unpack (List [Atom "RuleDelayed",a,b]) = Just (a,b)
unpack _ = Nothing

-- | unpack rule(s) arguemnts in function like Replace, ReplaceAll, etc.
unpackReplaceArg :: LispVal -> ThrowsError [Rule]
unpackReplaceArg val =
  let err = throwError (reps val)
      fromUnpackMaybe = maybe err return
  in
    case val of
      List (Atom "List":rules) ->
        fromUnpackMaybe (mapM unpack rules)
      rule -> fromUnpackMaybe (fmap return (unpack rule))
