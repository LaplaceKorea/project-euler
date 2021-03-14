module FunctionExtra where

import Data.Foldable (foldlM)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Linear.V2

twoDimListToMap :: (a -> b) -> Int -> Int -> Map (V2 Int) b -> [[a]] -> Map (V2 Int) b
twoDimListToMap _ _ _ !m [] = m
twoDimListToMap f r _ !m ([] : xs) = twoDimListToMap f (r + 1) 0 m xs
twoDimListToMap f r c !m ((x : xs) : xss) = twoDimListToMap f r (c + 1) (Map.insert (V2 r c) (f x) m) (xs : xss)
