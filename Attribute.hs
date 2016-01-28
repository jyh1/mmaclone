module Attribute where
import DataType

import qualified Data.Map.Strict as M
import Data.Maybe
-- import Data.List
-- attributes
data Attribute = HoldAll
                | HoldFirst
                | HoldRest
                | Orderless
                | Flatten
                | SequenceHold
    deriving (Show,Eq)
type Attributes = M.Map String [Attribute]

plusAttr :: [Attribute]
plusAttr = [Orderless, Flatten]

attributes :: Attributes
attributes = M.fromList[
              ("+", plusAttr),
              ("*", plusAttr),
              ("test", [SequenceHold,HoldAll])
              ]

lookUpAttribute :: String -> Attributes -> [Attribute]
lookUpAttribute name att = fromMaybe [] (M.lookup name att)

getAttributes :: LispVal -> Attributes -> [Attribute]
getAttributes (Atom name) att = lookUpAttribute name att
getAttributes _ _ = []
