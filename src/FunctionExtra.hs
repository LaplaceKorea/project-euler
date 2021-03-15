module FunctionExtra where

import Data.Foldable.Toolbox
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Linear.V2

twoDimListToMap :: (a -> b) -> Int -> Int -> Map (V2 Int) b -> [[a]] -> Map (V2 Int) b
twoDimListToMap _ _ _ !m [] = m
twoDimListToMap f r _ !m ([] : xs) = twoDimListToMap f (r + 1) 0 m xs
twoDimListToMap f r c !m ((x : xs) : xss) = twoDimListToMap f r (c + 1) (Map.insert (V2 r c) (f x) m) (xs : xss)

sumWhen :: (Foldable t, Ord a, Num a) => (a -> Bool) -> t a -> a
sumWhen p xs = sumOn (\a -> a * fromIntegral (fromEnum $ p a)) xs

productWhen :: (Foldable t, Ord a, Num a) => (a -> Bool) -> t a -> a
productWhen p xs = productOn (\a -> max 1 $ a * fromIntegral (fromEnum $ p a)) xs

maximumWhen :: (Foldable t, Ord a, Num a) => (a -> Bool) -> t a -> Maybe a
maximumWhen p xs = maximumOn (\a -> a * fromIntegral (fromEnum $ p a)) xs

minimumWhen :: (Foldable t, Ord a, Num a) => (a -> Bool) -> t a -> Maybe a
minimumWhen p xs = minimumOn (\a -> max 1 $ a * fromIntegral (fromEnum $ p a)) xs
