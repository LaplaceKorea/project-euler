module NumbersExtra
    ( module Control.Monad
    , module Data.List.Extra
    , module Data.Ratio
    , module Math.Combinat.Partitions.Integer
    , module Polynomials
    , count
    , sumOn, productOn
    , isPrime, primes, primeFactors, primePowerDecomposition, distinctPrimeFactors
    , ngons, triangles, squares, pentagons, hexagons, heptagons, octagons
      -- , ngonal, triangular, quadrilateral, pentagonal, hexagonal, heptagonal, octagonal
    , fibonaccis, fibonacciFrom
    , digits, undigits, backward
    , palindrome
    , pandigital0, pandigital1, pandigitals0, pandigitals1
    , divisors, numDivisors, properDivisors, totient
    , amicable, deficient, perfect, abundant
    , pythags
    , collatz
    , factorial, pascal, choose
    , reciprocal
    , cfConvergents, getCF
    , spiralDiagonals
    , narcissistic
    , repunit
    , stirling1, stirling2
    , sequentialPairs
    , pairwiseSequential
    )
where

import           Control.Arrow
import           Control.Monad
import           Data.List.Extra
import qualified Data.Numbers.Primes as P
import           Data.Ratio
import           Math.Combinat.Partitions.Integer
import qualified Math.Combinatorics.Exact.Binomial  as B
import qualified Math.Combinatorics.Exact.Factorial as F
import           Math.NumberTheory.Primes.Testing (isPrime)
import           Polynomials
import qualified Util (count)

-- | count b xs == genericLength (filter b xs)
count :: (Integral b) => (a -> Bool) -> [a] -> b
count = (fromIntegral .) . Util.count

sumOn, productOn :: (Num b) => (a -> b) -> [a] -> b
sumOn f = foldl' (\acc x -> acc + f x) 0
productOn f = foldl' (\acc x -> acc * f x) 1

primes :: [Integer]
primes = P.primes :: [Integer]

primeFactors :: Integer -> [Integer]
primeFactors = P.primeFactors

primePowerDecomposition :: Integer -> [(Integer, Integer)]
primePowerDecomposition = map (head &&& genericLength) . group . primeFactors

distinctPrimeFactors :: Integer -> [Integer]
distinctPrimeFactors = nubOrd . primeFactors

-- | `ngons s` generates the `s`-gonal numbers.
ngons :: Integer -> [Integer]
ngons s = ngons' s 1 (s - 1) where
    ngons' :: Integer -> Integer -> Integer -> [Integer]
    ngons' !s !n !m = n : ngons' s (n + m) (m + s - 2)

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
fibonaccis = fibonacciFrom 0 1

-- | `fibonacciFrom n0 n1` generates the Fibonacci sequence from the initial values `n0` and `n1`.
fibonacciFrom :: Integer -> Integer -> [Integer]
fibonacciFrom n0 n1 = n0 : fibonacciFrom n1 (n0 + n1)

-- | Turn a number into a list of its digits, most to least significant.
digits :: (Integral a, Read a, Show a) => a -> [a]
digits = map (read . pure) . show

-- | Make a number out of a list of digits, most to least significant.
undigits :: (Integral a, Read a, Show a) => [a] -> a
undigits = read . concatMap show

-- | Reverse the digits of a number.
backward :: (Integral a, Read a, Show a) => a -> a
backward = undigits . reverse . digits

pairwise :: (a -> a -> a) -> [a] -> [a]
pairwise f (x:y:xs) = f x y : pairwise f (y:xs)
pairwise _ xs = xs

palindrome :: (Integral a, Read a, Show a, Eq a) => a -> Bool
palindrome = ap (==) backward

-- | Does the number contain each digit from 0 to its length?
pandigital0 :: Integer -> Bool
pandigital0 n =
    let k = genericLength (digits n) in ([0..k-1]==) . sort $ digits n

-- | Does the number contain each digit from 1 to its length?
pandigital1 :: Integer -> Bool
pandigital1 n =
    let k = genericLength (digits n) in ([1..k]==) . sort $ digits n

-- | Find all 0-n pandigital numbers.
pandigitals0 :: Integer -> [Integer]
pandigitals0 n
    | n > 9     = error "not a digit"
    | n < 1     = error "invalid number"
    | otherwise = map undigits . filter ((/=0) . head) $ permutations [0..n]

-- | Find all 1-n pandigital numbers.
pandigitals1 :: Integer -> [Integer]
pandigitals1 n
    | n > 9     = error "not a digit"
    | n < 1     = error "invalid number"
    | otherwise = map undigits $ permutations [1..n]

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

-- | Euler's totient function.
totient :: Integer -> Integer
totient n
    | isPrime n = n - 1
    | otherwise = foldl' (\acc p -> acc - acc `div` p) n $ primeFactors n

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

factorial :: Integer -> Integer
factorial = F.factorial . fromIntegral

pascal :: (Integral a) => Int -> [a]
pascal = (iterate ((1:) . pairwise (+)) [1] !!)

-- choose :: (Integral a) => a -> a -> a
-- n `choose` m
--     | 0 <= m && m <= n = pascal n !! m
--     | otherwise = 0

choose :: Integer -> Integer -> Integer
choose n k = B.choose (fromIntegral n) (fromIntegral k)

-- | List the decimal expansion of the reciprocal of the number. Finite if and only if the expansion terminates.
reciprocal :: Integer -> [Integer]
reciprocal = reciprocal' 1 where
    reciprocal' :: Integer -> Integer -> [Integer]
    reciprocal' n x
        | n `mod` x == 0 = [n `div` x]
        | otherwise      = (last . digits) (n `div` x) : reciprocal' (10 * n) x

-- | `cfConvergents` takes a list of denominators and produces an infinite list of convergents. The continued fraction is assumed to be simple.
cfConvergents :: [Integer] -> [Rational]
cfConvergents [] = []
cfConvergents [x] = [x % 1]
cfConvergents (x:y:xs) = x % 1 : cfConvergents' (x * y + 1) y x 1 xs where
    cfConvergents' :: Integer -> Integer -> Integer -> Integer -> [Integer] -> [Rational]
    cfConvergents' pn qn pn1 qn1 []     = [pn % qn]
    cfConvergents' pn qn pn1 qn1 (x:xs) =
        let pn' = x * pn + pn1
            qn' = x * qn + qn1
        in  pn % qn : cfConvergents' pn' qn' pn qn xs

getCF :: (Eq a, RealFrac a) => a -> [Integer]
getCF (properFraction -> (i, f)) = i : if f == 0 then [] else getCF (recip f)


-- | `spiralDiagonals k` lists the numbers appearing on the diagonals of the Ulam spiral of side length `k`, in order from 1.
spiralDiagonals :: Integer -> [Integer]
spiralDiagonals k = spiralDiagonals' 0 0 [1 .. k ^ 2] where
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

-- | `repunit k` is the integer formed by repeating the digit 1 `k` times.
repunit :: Integer -> Integer
repunit = undigits . flip genericReplicate 1

stirling1, stirling2 :: Integer -> Integer -> Integer
stirling1 0 0 = 1
stirling1 _ 0 = 0
stirling1 0 _ = 0
stirling1 n k = (n-1) * stirling1 (n-1) k + stirling1 (n-1) (k-1)
stirling2 n k = (`div` factorial k) $ sum [ (if odd (k - j) then negate else id) $ choose k j * j^n | j <- [0..k] ]

sequentialPairs :: [a] -> [[a]]
sequentialPairs [] = []
sequentialPairs (x:xs) = sequentialPairs' x (x:xs) where
    sequentialPairs' :: a -> [a] -> [[a]]
    sequentialPairs' _ []  = []
    sequentialPairs' x [y] = [[y,x]]
    sequentialPairs' x (z:y:xs) = [z,y] : sequentialPairs' x (y:xs)

pairwiseSequential :: (a -> a -> Bool) -> [a] -> Bool
pairwiseSequential f xs | length xs > 1 = all (\[x,y] -> f x y) $ sequentialPairs xs
pairwiseSequential _ _ = True
