{-#LANGUAGE TemplateHaskell#-}
module Data.Environment.EnvironmentType where

import Data.DataType


import qualified Data.Map.Strict as M
import Control.Lens hiding (Context,List)
import qualified Data.Text as T
import Control.Monad.Trans.State



type ValueRule = M.Map LispVal LispVal
type PatternRule = [(LispVal, LispVal)]
type OwnValue = M.Map T.Text LispVal
type DownValue = M.Map T.Text Down
data Down = Down {_value :: ValueRule,_pattern :: PatternRule}
data Context = Context {_own :: OwnValue, _down :: DownValue}

makeLenses ''Down
makeLenses ''Context



-- * Types and common functions for defining primitive functions.

type Result = ThrowsError (Maybe LispVal)
type IOResult = IOThrowsError (Maybe LispVal)

type EvalResult = IOThrowsError LispVal

type StateResult a = StateT PrimiEnv IOThrowsError a

-- | Basic primitive function which only perform simple term rewriting
type Primi = StateResult LispVal

type Eval = LispVal -> Primi

type Primitives = M.Map T.Text Primi

type EvalArguments = [LispVal] -> Primi


-- | Envrionment for primitive function
data PrimiEnv = PrimiEnv
  { _eval :: Eval
  , _con :: Context
  , _args :: [LispVal]
  -- , _modified :: Bool
  , _dep :: Int
  , _line :: Int
  }

makeLenses ''PrimiEnv


-- Pattern matching types
type Pattern = LispVal
type Matched = (T.Text, LispVal)
type MatchRes = M.Map T.Text LispVal
initialMatch = M.empty


type Rule = (Pattern, LispVal)
type Rules = [Rule]

type MaybeMatch = Maybe MatchRes
type MatchResult = StateResult MaybeMatch
type ReplaceResult = StateResult (Maybe LispVal)
