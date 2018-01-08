module Tiles where

-- Typeclass repersenting tile merges
-- Minimal Complete Definition is all functions
-- newTile is a smart constructor
-- canMerge tells i two tiles can merge together
-- merge is the result of a two tile merge
-- score is the score after a merge
-- newTile generates a new tile randomly
class Tile a where
  canMerge :: a -> a -> Bool
  merge :: a -> a -> a
  score :: a -> a -> Int
  -- TODO Find the correct type signature for randomness
  -- newTile :: generator -> (generator, a)

-- 0 is the empty tile
newtype T2048 = T2048 Int deriving (Show)
newtype TDive = TDive Int deriving (Show)

instance Tile T2048 where
  canMerge (T2048 x) (T2048 y) = x == y
  merge (T2048 x) (T2048 y) = T2048 $ x + y
  score (T2048 x) (T2048 y) = min x y

-- TODO Figure out how Dive actually works
instance Tile TDive where
  canMerge (TDive x) (TDive y) = _
  merge (TDive x) (TDive y) = TDive $ x + y
  score (TDive x) (TDive y) = _
