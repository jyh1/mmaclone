module Data.Attribute where
import Data.DataType

import qualified Data.Map.Strict as M
import Data.Maybe
import Data.List(sort)
import qualified Data.Text as T
-- import Data.List
-- attributes
data Attribute = HoldAll
                | HoldFirst
                | HoldRest
                | Orderless
                | Flatten
                | SequenceHold
                | OneIdentity
    deriving (Show,Eq)
type Attributes = M.Map T.Text [Attribute]

plusAttr :: [Attribute]
plusAttr = [Orderless, Flatten,OneIdentity]

attributes :: Attributes
attributes = M.fromList[
              ("Plus", plusAttr),
              ("Times", plusAttr),
              ("Hold", [HoldAll]),
              ("Set", [HoldFirst,SequenceHold]),
              ("SetDelayed", [HoldAll,SequenceHold]),
              ("If", [HoldRest]),
              ("And",[HoldAll,OneIdentity]),
              ("Or",[HoldAll,OneIdentity]),
              ("Function", [HoldAll]),
              ("RuleDelayed",[HoldRest,SequenceHold]),
              ("Condition", [HoldAll]),
              ("Pattern", [HoldFirst])
              ]

lookUpAttribute :: T.Text -> Attributes -> [Attribute]
lookUpAttribute name att = fromMaybe [] (M.lookup name att)

getAttributes :: LispVal -> Attributes -> [Attribute]
getAttributes (Atom name) att = lookUpAttribute name att
getAttributes _ _ = []


-- attribute eval-----------------------------------------

allAttr :: [Attribute] -> LispVal-> [LispVal] -> [LispVal]
allAttr att h = attEvalOrderless att .attEvalFlatten att h .
                  attEvalSeqHold att

attEvalOrderless :: [Attribute] -> [LispVal] -> [LispVal]
attEvalOrderless att vals
  | Orderless `elem` att = sort vals
  | otherwise = vals

attEvalFlatten :: [Attribute] -> LispVal -> [LispVal] -> [LispVal]
attEvalFlatten att h vals
  | Flatten `elem` att = deleteSameHead vals h
  | otherwise = vals

attEvalSeqHold :: [Attribute] -> [LispVal] -> [LispVal]
attEvalSeqHold att vals
  | SequenceHold `elem` att = vals
  | otherwise = deleteSameHead vals (Atom "Sequence")

-- ------------------------------------------------
attributeTransform :: Attributes -> LispVal -> LispVal
attributeTransform att (List lis@(h:rest)) =
  let attrs = getAttributes h att in
    attTransOneIdent attrs lis
attributeTransform _ val = val

attTransOneIdent :: [Attribute] ->  [LispVal] -> LispVal
attTransOneIdent att lis
  | OneIdentity `elem` att && length lis == 2 = lis !! 1
  | otherwise = List lis
