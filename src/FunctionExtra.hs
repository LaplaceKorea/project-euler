module FunctionExtra where

import Data.Foldable (foldlM)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Linear.V2

takeWhileUnique :: (Eq a) => [a] -> [a]
takeWhileUnique = either reverse reverse . foldlM unique []
  where
    unique :: (Eq a) => [a] -> a -> Either [a] [a]
    unique acc x =
        if x `elem` acc
            then Left acc
            else Right $ x : acc

dropWhileUnique :: (Eq a) => [a] -> [a]
dropWhileUnique = drop . length . takeWhileUnique <*> id

iterateWhileUnique :: (Eq a) => (a -> a) -> a -> [a]
iterateWhileUnique = iterateWhileUnique' []
  where
    iterateWhileUnique' :: (Eq a) => [a] -> (a -> a) -> a -> [a]
    iterateWhileUnique' !acc f !x
        | x `elem` acc = acc
        | otherwise = iterateWhileUnique' (acc ++ [x]) f (f x)

takeWhileUniqueOrd :: (Eq a, Ord a) => [a] -> [a]
takeWhileUniqueOrd = either (reverse . fst) (reverse . fst) . foldlM unique ([], Set.empty)
  where
    unique :: (Eq a, Ord a) => ([a], Set a) -> a -> Either ([a], Set a) ([a], Set a)
    unique (l, acc) x =
        if x `Set.member` acc
            then Left (l, acc)
            else Right (x : l, Set.insert x acc)

dropWhileUniqueOrd :: (Eq a, Ord a) => [a] -> [a]
dropWhileUniqueOrd = drop . length . takeWhileUniqueOrd <*> id

twoDimListToMap :: (a -> b) -> Int -> Int -> Map (V2 Int) b -> [[a]] -> Map (V2 Int) b
twoDimListToMap _ _ _ !m [] = m
twoDimListToMap f r _ !m ([] : xs) = twoDimListToMap f (r + 1) 0 m xs
twoDimListToMap f r c !m ((x : xs) : xss) = twoDimListToMap f r (c + 1) (Map.insert (V2 r c) (f x) m) (xs : xss)

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x : xs) = map (x :) (subsets xs) ++ subsets xs
