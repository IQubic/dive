module Tile where

-- Typeclass repersenting tile merges
-- Minimal Complete Definition is all functions
-- newTile is a smart constructor
-- canMerge tells if two tiles can merge together
-- merge is the result of a two tile merge
-- score is the score after a merge
-- newTile generates a new tile randomly
class Tile a where
  newTile :: Int -> Maybe a
  canMerge :: a -> a -> Bool
  merge :: a -> a -> a
  score :: a -> a -> Int

-- 0 is the empty tile
newtype T2048 = T2048 Int deriving (Show)
newtype TDive = TDive Int deriving (Show)

instance Tile T2048 where
  newTile x = if x < 0 then Nothing else Just $ T2048 x
  canMerge (T2048 x) (T2048 y) = x == y
  merge (T2048 x) (T2048 y) = T2048 $ x + y
  score (T2048 x) (T2048 y) = x + y

instance Tile TDive where
  newTile x = if x < 0 then Nothing else Just $ TDive x
  canMerge (TDive x) (TDive y) = x `mod` y == 0 || y `mod`x == 0
  merge (TDive x) (TDive y) = TDive $ x + y
  score (TDive x) (TDive y) = min x y
