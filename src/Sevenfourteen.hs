module Sevenfourteen where

import NumbersExtra
import Data.Time.Clock
import Data.Time.Clock.System

unodigit :: Integer -> Bool
unodigit = allSame . digits

duodigit :: Integer -> Bool
-- duodigit = (<=2) . length . nubOrd . digits
duodigit = (\(x:xs) -> allSame $ filter (/=x) xs) . digits

d :: Integer -> Integer
d 580 = 22022020
d 590 = 110111110
d 790 = 100100110
d 810 = 9999999990
d 860 = 22022020
d 890 = 110101010
d 970 = 111000010
d 1030 = 111000010
d 1070 = 1000100110
d 1090 = 10010101110
d 1270 = 110100110
d 1390 = 1101101010
d 1570 = 101011010110
d 1580 = 200200220
d 1620 = 22222222020
d n = head . dropWhile (not . duodigit) $ iterate (+n) n

bigD :: Integer -> Integer
bigD = sumOn d . enumFromTo 1

-- q714_test :: IO ()
-- q714_test = do
--     t1 <- systemToUTCTime <$> getSystemTime
--     putStr $ "bigD 1000: " ++ show (bigD 1000)
--     t2 <- systemToUTCTime <$> getSystemTime
--     putStrLn $ " (" ++ show (diffUTCTime t2 t1) ++ " s)"
--     t3 <- systemToUTCTime <$> getSystemTime
--     putStr $ "bigD' 1000: " ++ show (bigD' 1000)
--     t4 <- systemToUTCTime <$> getSystemTime
--     putStrLn $ " (" ++ show (diffUTCTime t4 t3) ++ " s)"
