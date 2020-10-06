module FunctionExtra where

import Data.Foldable (foldlM)
import Data.Set (Set)
import Data.Set qualified as Set

takeWhileUnique :: (Eq a) => [a] -> [a]
takeWhileUnique = either reverse reverse . foldlM unique [] where
    unique :: (Eq a) => [a] -> a -> Either [a] [a]
    unique acc x =
        if x `elem` acc
            then Left acc
            else Right $ x : acc

dropWhileUnique :: (Eq a) => [a] -> [a]
dropWhileUnique = drop . length . takeWhileUnique <*> id

iterateWhileUnique :: (Eq a) => (a -> a) -> a -> [a]
iterateWhileUnique = iterateWhileUnique' [] where
    iterateWhileUnique' :: (Eq a) => [a] -> (a -> a) -> a -> [a]
    iterateWhileUnique' !acc f !x
        | x `elem` acc = acc
        | otherwise = iterateWhileUnique' (acc ++ [x]) f (f x)

takeWhileUniqueOrd :: (Eq a, Ord a) => [a] -> [a]
takeWhileUniqueOrd = either (reverse . fst) (reverse . fst) . foldlM unique ([], Set.empty) where
    unique :: (Eq a, Ord a) => ([a], Set a) -> a -> Either ([a], Set a) ([a], Set a)
    unique (l, acc) x =
        if x `Set.member` acc
            then Left (l, acc)
            else Right (x : l, Set.insert x acc)

dropWhileUniqueOrd :: (Eq a, Ord a) => [a] -> [a]
dropWhileUniqueOrd = drop . length . takeWhileUniqueOrd <*> id