module Hier where

import Data.Function

class Hier a where
  upgrade :: a -> a
  downgrade :: a -> a
  rank :: a -> Int
  peerQ :: a -> a -> Bool
  peerQ = (==) `on` rank
  superQ :: a -> a -> Bool
  superQ = (>) `on` rank
  inferQ :: a -> a -> Bool
  inferQ = (<) `on` rank

peerOpUp :: (Hier a) => (a -> a -> b) -> a -> a -> b
peerOpUp op a1 a2
  | superQ a1 a2 = peerOpUp op a1 (upgrade a2)
  | inferQ a1 a2 = peerOpUp op (upgrade a1) a2
  | otherwise = op a1 a2

peerOpDown :: (Hier a) => (a -> a -> b) -> a -> a -> b
peerOpDown op a1 a2
  | superQ a1 a2 = peerOpDown op (downgrade a1) a2
  | inferQ a1 a2 = peerOpDown op a1 (downgrade a2)
  | otherwise = op a1 a2
