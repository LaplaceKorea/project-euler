module NumbersExtra
    ( module Control.Monad
    , module Data.List.Extra
    , module Data.Ratio
    , module Math.Combinat.Partitions.Integer
    , module Polynomials
    , count
    , isPrime
    , primes
    , primeFactors
    , distinctPrimeFactors
    , ngons
    , triangles
    , squares
    , pentagons
    , hexagons
    , heptagons
    , octagons
      -- , ngonal, triangular, quadrilateral, pentagonal, hexagonal, heptagonal, octagonal
    , fibonaccis
    , fibonacciSequence
    , digits
    , undigits
    , backward
    , palindrome
    , pandigital0
    , pandigital1
    , divisors
    , numDivisors
    , properDivisors
    , amicable
    , deficient
    , perfect
    , abundant
    , pythags
    , collatz
    , factorial
    , choose
    , reciprocal
    , continuedFraction
    , spiralDiagonals
    , narcissistic
    , repunit
    )
where

import           Control.Monad
import           Data.List.Extra
import qualified Data.Numbers.Primes as P
import           Data.Ratio
import           Math.Combinat.Partitions.Integer
import           Math.Combinatorics.Exact.Binomial
import           Math.Combinatorics.Exact.Factorial
import           Math.NumberTheory.Primes.Testing (isPrime)
import           Polynomials
import qualified Util

-- | count b xs == genericLength (filter b xs)
count :: (Integral b) => (a -> Bool) -> [a] -> b
count = (fromIntegral .) . Util.count

primes :: [Integer]
primes = P.primes :: [Integer]

primeFactors :: Integer -> [Integer]
primeFactors = P.primeFactors

distinctPrimeFactors :: Integer -> [Integer]
distinctPrimeFactors = nubOrd . primeFactors

-- | `ngons s` generates the `s`-gonal numbers.
ngons :: Integer -> [Integer]
ngons s = ngons' s 1 (s - 1) where
    ngons' :: Integer -> Integer -> Integer -> [Integer]
    ngons' s n m = n : ngons' s (n + m) (m + s - 2)

triangles :: [Integer]
triangles = ngons 3
squares :: [Integer]
squares = ngons 4
pentagons :: [Integer]
pentagons = ngons 5
hexagons :: [Integer]
hexagons = ngons 6
heptagons :: [Integer]
heptagons = ngons 7
octagons :: [Integer]
octagons = ngons 8

-- | The traditional Fibonacci sequence with initial terms 0 and 1.
fibonaccis :: [Integer]
fibonaccis = fibonacciSequence 0 1

-- | `fibonacciSequence n0 n1` generates the Fibonacci sequence from the initial values `n0` and `n1`.
fibonacciSequence :: Integer -> Integer -> [Integer]
fibonacciSequence n0 n1 = n0 : fibonacciSequence n1 (n0 + n1)

-- | Turn a number into a list of its digits, most to least significant.
digits :: (Integral a, Read a, Show a) => a -> [a]
digits = map (read . pure) . show

-- | Make a number out of a list of digits, most to least significant.
undigits :: (Integral a, Read a, Show a) => [a] -> a
undigits = read . concatMap show

-- | Reverse the digits of a number.
backward :: (Integral a, Read a, Show a) => a -> a
backward = undigits . reverse . digits

palindrome :: (Integral a, Read a, Show a, Eq a) => a -> Bool
palindrome = ap (==) backward

-- | Does the number contain each digit from 0 to its length?
pandigital0 :: Integer -> Bool
pandigital0 n =
    let k = genericLength (digits n) in null ([0 .. k] \\ digits n)

-- | Does the number contain each digit from 1 to its length?
pandigital1 :: Integer -> Bool
pandigital1 n =
    let k = genericLength (digits n) in null ([1 .. k] \\ digits n)

-- | Find all the divisors of a number.
divisors :: Integer -> [Integer]
divisors n =
    let k = ceiling (sqrt $ fromIntegral n)
    in  nubSort $ concatMap
            (\(m, k) -> if m == k then [m] else [m, k])
            [ (m, n `div` m) | m <- [1 .. k], n `mod` m == 0 ]

-- | Find all the divisors of a number, excluding itself.
properDivisors :: Integer -> [Integer]
properDivisors n = divisors n \\ [n]

-- | Find the number of divisors of a number, including itself.
numDivisors :: Integer -> Integer
numDivisors = genericLength . divisors

-- | Amicable numbers *n* are not equal to their proper divisor sum, *d(n)*, but have the property *d(d(n)) = n*.
amicable :: Integer -> Bool
amicable n = n /= d n && d (d n) == n where d = sum . properDivisors

-- | Is the number greater than the sum of its proper divisors?
deficient :: Integer -> Bool
deficient n = n > 1 && n > sum (properDivisors n)

-- | Is the number equal to the sum of its proper divisors?
perfect :: Integer -> Bool
perfect n = n > 1 && n == sum (properDivisors n)

-- | Is the number less than the sum of its proper divisors?
abundant :: Integer -> Bool
abundant n = n > 1 && n < sum (properDivisors n)

-- | An infinite list of Pythagorean triples.
pythags :: [(Integer, Integer, Integer)]
pythags =
    [ (a, b, c)
    | c <- [1 ..]
    , b <- [1 .. c]
    , a <- [1 .. b]
    , a ^ 2 + b ^ 2 == c ^ 2
    ]

-- | Generate the number's hailstone sequence.
collatz :: Integer -> [Integer]
collatz n | even n    = n : collatz (n `div` 2)
          | n == 1    = [n]
          | otherwise = n : collatz (3 * n + 1)

-- | List the decimal expansion of the reciprocal of the number. Finite if and only if the expansion terminates.
reciprocal :: Integer -> [Integer]
reciprocal = reciprocal' 1 where
    reciprocal' :: Integer -> Integer -> [Integer]
    reciprocal' n x
        | n `mod` x == 0 = [n `div` x]
        | otherwise      = (last . digits) (n `div` x) : reciprocal' (10 * n) x

-- | `continuedFraction outside num dens` produces an infinite list
continuedFraction :: Integer -> Integer -> [Integer] -> [Rational]
continuedFraction outside numer denoms = outside % 1 : continuedFraction' outside numer denoms (numer % head denoms) where
    continuedFraction' :: Integer -> Integer -> [Integer] -> Rational -> [Rational]
    continuedFraction' outside _     []     frac = [outside % 1 + frac]
    continuedFraction' outside numer denoms frac =
        let newdenom = fromIntegral (head denoms) + frac
            newfrac  = fromIntegral numer / newdenom
        in  outside % 1 + newfrac : continuedFraction' outside numer (tail denoms) newfrac

-- | `spiralDiagonals k` lists the numbers appearing on the diagonals of the Ulam spiral of side length `k`, in order from 1.
spiralDiagonals :: Integer -> [Integer]
spiralDiagonals k = spiralDiagonals' 0 0 [1 .. k ^ 2]
  where
    spiralDiagonals' _ _ [] = []
    spiralDiagonals' n 0 xs = case genericDrop (n - 1) xs of
        []       -> []
        (y : ys) -> y : spiralDiagonals' (n + 2) 3 ys
    spiralDiagonals' n k xs = case genericDrop (n - 1) xs of
        []       -> []
        (y : ys) -> y : spiralDiagonals' n (k - 1) ys

-- | `narcissistic k n` tests whether `n` is equal to the sum of the `k`th powers of its digits.
narcissistic :: Integer -> Integer -> Bool
narcissistic k n = (== n) . sum . map (^ k) $ digits n

-- | `repunit k` integer formed by repeating the digit 1 `k` times.
repunit :: Integer -> Integer
repunit = undigits . flip genericReplicate 1
