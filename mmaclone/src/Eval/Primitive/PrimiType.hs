{-#LANGUAGE FlexibleContexts , TemplateHaskell#-}

module Eval.Primitive.PrimiType where

import Data.DataType
import Data.Number.Number
import Data.Environment.Environment

import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.State
import Control.Lens
import Control.Applicative((<$>))

-- * Types and common functions for defining primitive functions.

type Result = ThrowsError (Maybe LispVal)
type IOResult = IOThrowsError (Maybe LispVal)

type EvalResult = IOThrowsError LispVal
type Eval = LispVal -> EvalResult


-- | Envrionment for primitive function
data PrimiEnv = PrimiEnv
  { _eval :: Eval
  , _env :: Env
  , _args :: [LispVal]
  , _modified :: Bool
  }

makeLenses ''PrimiEnv

-- Basic primitive function which only perform simple term rewriting
type Primi = StateT PrimiEnv IOThrowsError LispVal
-- -- | Primitive function which will likely modifying enviroment or doing IO
-- type IOPrimi = Env -> [LispVal] -> IOResult
-- -- | Primitive function which would evaluate LispVal internally
-- type EvalPrimi = Eval -> [LispVal] -> IOResult
--
-- type SingleFun = LispVal -> Result
-- type BinaryFun = LispVal -> LispVal -> Result
-- type IOBinary = Env -> LispVal -> LispVal -> IOResult

type Primitives = M.Map String Primi

-- | Pack a LispVal in IOResult or Result
-- hasValue :: (Monad m) => LispVal -> m (Maybe LispVal)
-- hasValue = return . Just

-- | Indicating that the evaluation will not provide new result
-- noChange :: (Monad m) => m (Maybe LispVal)
-- noChange = return Nothing

-- | The most genenral function to constraint the arguments number of
-- primitive function
checkArgsNumber :: (Int -> Bool) -> (LispVal -> Int -> IOThrowsError ()) ->
  StateT PrimiEnv IOThrowsError ()
checkArgsNumber check throw = do
  num <- (\x -> x - 1) <$> uses args length
  unless (check num) $ do
    name <- uses args head
    lift (throw name num)

-- | expects more than n arguments.
manynop n = checkArgsNumber (>= n) throw
  where throw val x = throwError (NumArgsMore (unpackAtom val) n x)

-- | expect more than one arugments
many1op = manynop 1

-- | argument list length is between l and r.
between l r = checkArgsNumber (\x -> x >= l && x <= r) throw
  where throw val x = throwError (NumArgsBetween (unpackAtom val) l r x)

-- | Ensure that the argument list has excatly n elements.
withnop n = checkArgsNumber (== n) throw
  where throw val x = throwError (NumArgs (unpackAtom val) n x)

-- | evaluate a LispVal with function in PrimiEnv context
evaluate :: LispVal -> Primi
evaluate val = do
  evalFun <- use eval
  lift $ evalFun val
