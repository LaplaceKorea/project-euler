module Sevenfifteen where

import NumbersExtra
import Control.Concurrent.Async

f' :: Integer -> [(Integer, Integer, Integer, Integer, Integer, Integer)]
f' n =
    [ (a,b,c,d,e,f)
    | a <- [0..n-1]
    , b <- [0..n-1]
    , c <- [0..n-1]
    , d <- [0..n-1]
    , e <- [0..n-1]
    , f <- [0..n-1]
    , gcd (sum $ map (^2) [a,b,c,d,e,f]) n == 1 ]

eff :: (Integer, Integer) -> Integer
eff (2, e) = 2^(6 * e - 1)
eff (p, e) = (p - 1) * p^(6 * e - 4) * (p^3 - (-1)^(3 * (p - 1) `div` 2))

f :: Integer -> Integer
f = productOn eff . primeFactorPairs

tot :: (Integer, Integer) -> Integer
tot (p, e) = (p - 1) * p^(e - 1)

totient' :: Integer -> Integer
totient' = productOn tot . primeFactorPairs

divEffTot :: (Integer, Integer) -> Integer
divEffTot (2, e) = 2^(5 * e - 2)
divEffTot (p, e) = p^(5 * (e - 1)) * (p^3 - (-1)^(3 * (p - 1) `div` 2))

gee :: Integer -> Integer
gee = liftM2 div (productOn (liftM2 div eff tot) . primeFactorPairs) (^2)

geez :: Integer -> Integer
geez 0 = 0
geez n = gee n + geez (n-1)

g :: Integer -> Integer
g n = answerMod $ sumOn gee [1..n]

-- g :: Integer -> Integer
-- g n = sum $ map (\k -> f k `div` (k^2 * totient k)) [1..n]

answerMod :: Integer -> Integer
answerMod = (`mod` 1000000007)