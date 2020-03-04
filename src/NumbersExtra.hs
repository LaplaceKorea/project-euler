module NumbersExtra
    ( module Control.Monad
    , module Data.List.Extra
    , module Data.Ratio
    , module Math.Combinat.Partitions.Integer
    , module Polynomials
    , count
    , isPrime, primes, primeFactors
    , ngons, triangles, squares, pentagons, hexagons, heptagons, octagons
    -- , ngonal, triangular, quadrilateral, pentagonal, hexagonal, heptagonal, octagonal
    , fibonaccis, fibonacciSequence
    , digits, undigits, backwards, palindrome
    , divisors, numDivisors, properDivisors, amicable, deficient, perfect, abundant
    , pythags
    , collatz
    , factorial, choose
    , reciprocal, continuedFraction
    , spiralDiagonals
    , narcissistic
    , repunit
    ) where


import           Control.Monad
import           Data.List.Extra
import           Data.Numbers.Primes (isPrime, primes, primeFactors)
import           Data.Ratio
import           Math.Combinat.Partitions.Integer
import           Math.Combinatorics.Exact.Binomial
import           Math.Combinatorics.Exact.Factorial
import           Polynomials
import qualified Util


count :: (a -> Bool) -> [a] -> Integer
count = (fromIntegral .) . Util.count


ngons :: Integer -> [Integer]
ngons s = ngons' s 1 (s-1) where
    ngons' s n m = n : ngons' s (n+m) (m+s-2)
triangles, squares, pentagons, hexagons, heptagons, octagons :: [Integer]
triangles = ngons 3
squares   = ngons 4
pentagons = ngons 5
hexagons  = ngons 6
heptagons = ngons 7
octagons  = ngons 8


fibonaccis :: [Integer]
fibonaccis = fibonacciSequence 0 1
fibonacciSequence :: Integer -> Integer -> [Integer]
fibonacciSequence n0 n1 = n0 : fibonacciSequence n1 (n0 + n1)


digits :: (Integral a, Read a, Show a) => a -> [a]
digits = map (read . pure) . show
undigits :: (Integral a, Read a, Show a) => [a] -> a
undigits = read . concatMap show
backwards :: (Integral a, Read a, Show a) => a -> a
backwards = undigits . reverse . digits
palindrome :: (Integral a, Read a, Show a, Eq a) => a -> Bool
palindrome = ap (==) backwards


divisors :: Integer -> [Integer]
divisors n = let k = ceiling (sqrt $ fromIntegral n) in nubSort $ concatMap (\(m, k) -> if m == k then [m] else [m, k]) [ (m, n `div` m) | m <- [1..k], n `mod` m == 0 ]
properDivisors :: Integer -> [Integer]
properDivisors n = divisors n \\ [n]
numDivisors :: Integer -> Integer
numDivisors = genericLength . divisors
amicable :: Integer -> Bool
amicable n = n /= d n && d (d n) == n where d = sum . properDivisors
deficient, perfect, abundant :: Integer -> Bool
deficient n = n > 1 && n >  sum (properDivisors n)
perfect   n = n > 1 && n == sum (properDivisors n)
abundant  n = n > 1 && n <  sum (properDivisors n)


pythags :: [(Integer, Integer, Integer)]
pythags = [ (a, b, c) | c <- [1..], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2 ]


collatz :: Integer -> [Integer]
collatz n | even n    = n : collatz (n `div` 2)
          | n == 1    = [n]
          | otherwise = n : collatz (3 * n + 1)


reciprocal :: Integer -> [Integer]
reciprocal = reciprocal' 1 where
    reciprocal' :: Integer -> Integer -> [Integer]
    reciprocal' n x
        | n `mod` x == 0 = [n `div` x]
        | otherwise      = (last . digits) (n `div` x) : reciprocal' (10 * n) x
continuedFraction :: Integer -> Integer -> [Integer] -> [Rational]
continuedFraction outside numer denoms = outside % 1 : continuedFraction' outside numer denoms (numer % (head denoms)) where
    continuedFraction' :: Integer -> Integer -> [Integer] -> Rational -> [Rational]
    continuedFraction' outside _     []     frac = [outside % 1 + frac]
    continuedFraction' outside numer denoms frac =
        let newdenom = fromIntegral (head denoms) + frac
            newfrac  = fromIntegral numer / newdenom
        in  outside % 1 + newfrac : continuedFraction' outside numer (tail denoms) newfrac


spiralDiagonals :: Integer -> [Integer]
spiralDiagonals k = spiralDiagonals' 0 0 [1..k^2] where
    spiralDiagonals' _ _ [] = []
    spiralDiagonals' n 0 xs = case genericDrop (n-1) xs of
        []     -> []
        (y:ys) -> y : spiralDiagonals' (n+2) 3 ys
    spiralDiagonals' n k xs = case genericDrop (n-1) xs of
        []     -> []
        (y:ys) -> y : spiralDiagonals' n (k-1) ys


narcissistic :: Integer -> Integer -> Bool
narcissistic k n = (==n) . sum . map (^k) $ digits n

repunit :: Integer -> Integer
repunit = undigits . flip genericReplicate 1