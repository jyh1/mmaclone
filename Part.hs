module Part(part) where
import DataType
import Number

import Control.Monad
import Control.Monad.Except

part :: [LispVal] -> Result
part l = case partWithPartError l of
  Left err -> throwError (fromPartError err (list (Atom "Part":l)))
  Right val -> hasValue val

data PartSpeci = S Int | L [Int]
data PartRes = Sres LispVal | Lres [LispVal]
data PartError = Pkspec | Partw | Partd
type ThrowPart = Either PartError

partWithPartError :: [LispVal] -> ThrowPart LispVal
partWithPartError (l:ls) = do
  speci <- toPartSpeci ls
  partWithSpeci l speci

toPartSpeci :: [LispVal] -> ThrowPart [PartSpeci]
toPartSpeci = mapM toPartSpeci'
  where
    toPartSpeci' (List (Atom "List":res)) = liftM L $ mapM unpack res
    toPartSpeci' n = liftM S $ unpack n
    unpack :: LispVal -> ThrowPart Int
    unpack (Number (Integer n)) = return (fromIntegral n)
    unpack _ = throwError Pkspec

partWithSpeci :: LispVal -> [PartSpeci] -> ThrowPart LispVal
partWithSpeci l [] = return l
partWithSpeci l (s:ss) = do
  parted <- partOnce l s
  case parted of
    Sres res -> partWithSpeci res ss
    Lres reses -> liftM list $ mapM (`partWithSpeci` ss) reses

partOnce :: LispVal -> PartSpeci -> ThrowPart PartRes
partOnce lv (S n) = liftM Sres $ getPart lv n
partOnce lv (L ns) = liftM Lres $ mapM (getPart lv) ns

getPart :: LispVal -> Int -> ThrowPart LispVal
getPart (List lis) n =
  if length lis < n+1 then throwError Partw
    else return $ lis !! n
getPart _ _ = throwError Partd

-- partNest :: PartRes -> PartSpeci -> ThrowPart PartRes
-- partNest (Sres l) ps = partOnce l ps
-- partNest (Lres ls) ps =
--   let parted = mapM (liftM fromRes . flip partOnce ps) ls in
--     liftM Lres parted

-- fromRes :: PartRes -> LispVal
-- fromRes (Sres n) = n
-- fromRes (Lres n) = list n

-- fromList :: LispVal -> [LispVal]
-- fromList (List v) = v

fromPartError :: PartError -> LispVal -> LispError
fromPartError Partd = PartE "part specification is longer than depth of object"
fromPartError Partw = PartE "part specification does not exist"
fromPartError Pkspec = PartE "invalid part specification"
