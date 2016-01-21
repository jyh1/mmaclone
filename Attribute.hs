module Attribute where

-- attributes
data Attribute = HoldAll
                | HoldFirst
                | HoldRest
                | Orderless
    deriving (Show)
type Attributes = [Attribute]

attributes :: [(String ,Attributes)]
attributes = [
              -- ("ss", [HoldAll])
              ]
