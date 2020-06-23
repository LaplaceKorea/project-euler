module Sev09 where

import NumbersExtra
import Debug.Trace
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Time.Clock
import Data.Time.Clock.System

data Bag = Bag { bagID :: Integer, contents :: Either () [Bag] } deriving (Eq)

instance Show Bag where
    show (Bag bagID contents) = show bagID ++ case contents of
        Left ()  -> ""
        Right bs -> ' ' : show bs

newtype Cupboard = Cupboard { unCupboard :: [Bag] } deriving (Eq)

instance Show Cupboard where
    show = show . unCupboard

lastBag :: Cupboard -> Integer
lastBag = maximum . map bagID . unCupboard

nextDay :: Cupboard -> [Cupboard]
nextDay (Cupboard [])   = [Cupboard [Bag 1 (Left ())]]
nextDay c@(Cupboard bs) =
    let inThisBag  = Bag (lastBag c + 1)
        bagOptions = filter (liftM2 (&&) (>0) even . length) $ subsequences bs
    in  map Cupboard $ (inThisBag (Left ()) : bs) : map (\stored -> inThisBag (Right stored) : (bs \\ stored)) bagOptions

day :: Int -> [Cupboard]
day 0 = [Cupboard []]
day n = concatMap nextDay . day $ pred n

f :: Int -> Int
f = length . day

risfac :: (Num a, Enum a) => a -> a -> a
risfac x k = product [x,x+1..x+k-1]

andre :: Integer -> Integer
andre r =
    let ar | odd r     = (if odd ((r - 1) `div` 2) then negate else id) $ 1 + (1 % 2)^r
           | otherwise = if odd (r `div` 2) then -1 else 1
    in  (`mod` 1020202009) . numerator $ negate (4^r * (denominator ar % numerator ar)) * sum [ (if odd k then negate else id) $ (stirling2 r k % (k + 1)) * risfac (3 % 4) (k % 1) | k <- [1..r] ]

andre' :: Integer -> Integer
andre' 0 = 1
andre' n = sum [ choose (n-1) k * andre k * andre (n-1-k) | k <- [0..n-1] ] `div` 2

-- a000111 0 = 1
-- a000111 n = (`mod` 1020202009) . last $ a008281_tabl !! n

a008280 n k = a008280_tabl !! n !! k
a008280_row n = a008280_tabl !! n
a008280_tabl = ox True a008281_tabl where
    ox turn (xs:xss) = (if turn then reverse xs else xs) : ox (not turn) xss

a008281_tabl = iterate (scanl (+) 0 . reverse) [1]

a008281 :: Integer -> Seq (Seq Integer)
a008281 n = S.iterateN (fromInteger n) (S.scanl (+) 0 . S.reverse) (S.singleton 1)

a000111 :: Integer -> Integer
a000111 n = (`mod` 1020202009) $ case S.viewr (a008281 $ n + 1) of
    S.EmptyR -> 0
    _ S.:> a -> case S.viewr a of
        S.EmptyR -> 0
        _ S.:> b -> b

q709_test :: IO ()
q709_test = do
    t1 <- systemToUTCTime <$> getSystemTime
    putStr $ "24: " ++ show (a000111 24)
    t2 <- systemToUTCTime <$> getSystemTime
    putStrLn $ " (" ++ show (diffUTCTime t2 t1) ++ " s)"
    t3 <- systemToUTCTime <$> getSystemTime
    putStr $ "246: " ++ show (a000111 246)
    t4 <- systemToUTCTime <$> getSystemTime
    putStrLn $ " (" ++ show (diffUTCTime t4 t3) ++ " s)"
    t5 <- systemToUTCTime <$> getSystemTime
    putStr $ "2468: " ++ show (a000111 2468)
    t6 <- systemToUTCTime <$> getSystemTime
    putStrLn $ " (" ++ show (diffUTCTime t6 t5) ++ " s)"

q709 :: Integer
q709 = a000111 24680