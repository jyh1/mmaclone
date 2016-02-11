module Eval.Primitive.Primi.Compare.Compare
        (equall,lessl,lessEquall,greaterl,greaterEquall) where
import Data.DataType
import Data.Number.Number
import Eval.Primitive.PrimiType

import Control.Monad
import Control.Monad.Except

equall = binop "Equal" equal
lessl = binop "Less" less
lessEquall = binop "LessEqual" lessEqual
greaterl = binop "Greater" greater
greaterEquall = binop "GreaterEqual" greaterEqual


-- unpackCompare :: LispVal -> LispVal ->
--                   Unpacker -> ThrowsError Ordering
-- unpackCompare a b (Unpacker unpack) = do
--   unpacka <- unpack a
--   unpackb <- unpack b
--   return $  compare unpacka unpackb
--
-- getCompareResult :: LispVal -> LispVal -> ThrowsError Ordering
-- getCompareResult a b =
--   sumError $ map (unpackCompare a b) unpackers
--
getBoolResult :: [Ordering] -> BinaryFun
getBoolResult e (Number a) (Number b) =
  return $ Just (toBool $ numberComp a b `elem` e)
getBoolResult _ _ _ = return Nothing

equal :: BinaryFun
equal = getBoolResult [EQ]

less :: BinaryFun
less = getBoolResult [LT]

lessEqual,greater,greaterEqual :: BinaryFun
lessEqual = getBoolResult [LT,EQ]
greater = getBoolResult [GT]
greaterEqual = getBoolResult [GT,EQ]

-- internalBoolOp :: (Bool ->Bool -> Bool) -> Result -> Result -> Result
-- internalBoolOp f =
--   liftM2 f''
--     where
--       f'' = liftM2 f'
--       f' a1 a2 = toBool (f (unBool a1) (unBool a2))
--
-- internalAnd = internalBoolOp (&&)
-- internalOr = internalBoolOp (||)
--
-- internalNot :: Result -> Result
-- internalNot=
--   liftM f''
--     where f'' = liftM f'
--           f' a = toBool $ not (unBool a)
