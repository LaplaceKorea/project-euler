module Sev20 where

import Control.Monad (ap)
import NumbersExtra

arithmeticProgression :: [Integer] -> Bool
arithmeticProgression (x:y:xs) = arithmeticProgression' (y - x) (y:xs) where
    arithmeticProgression' :: Integer -> [Integer] -> Bool
    arithmeticProgression' k (x:y:xs) = y - x == k && arithmeticProgression' k (y:xs)
    arithmeticProgression' _ _        = True
arithmeticProgression _ = True

avoiding :: Integer -> [Integer] -> Bool
avoiding k = not . any arithmeticProgression . filter ((==k) . genericLength) . subsequences

s :: Integer -> Maybe Integer
s = fmap (fromIntegral . succ) . findIndex (avoiding 3) . sort . permutations . enumFromTo 1