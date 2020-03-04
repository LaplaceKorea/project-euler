module CalendarExtra
    ( Time(..), time, secondsSinceMidnight
    , Date(..), date, daysSinceJan1
    , Weekday(..)
    , yearCalendar
    , weekday
    ) where

import Data.List
import Data.Maybe
import NumbersExtra

justIf :: a -> Bool -> Maybe a
x `justIf` b = if b then Just x else Nothing

data Time = Time { hours :: Integer, minutes :: Integer, seconds :: Integer } deriving (Eq, Ord)

time :: Integer -> Integer -> Integer -> Maybe Time
time h m s = justIf (Time h m s)
           $  0 <= h && h <= 23
           && 0 <= m && m <= 59
           && 0 <= s && s <= 61

instance Show Time where
    show (Time h m s) =
        let nh = length (digits h)
            nm = length (digits m)
            ns = length (digits s)
        in  replicate (2 - nh) '0' ++ show h ++ ":" ++ replicate (2 - nm) '0' ++ show m ++ ":" ++ replicate (2 - ns) '0' ++ show s

secondsSinceMidnight :: Time -> Integer
secondsSinceMidnight (Time h m s) = 3600 * h + 60 * m + s

data Date    = Date { year :: Integer, month :: Integer, day :: Integer, dayTime :: Time } deriving (Ord)
instance Eq Date where
    (Date y m d _) == (Date y' m' d' _) = y == y' && m == m' && d == d'

data Weekday = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday deriving (Eq, Ord, Show, Enum)

instance Show Date where
    show (Date y m d t) =
        let ny = length (digits y)
            nm = length (digits m)
            nd = length (digits d)
        in  replicate (4 - ny) '0' ++ show y ++ "-" ++ replicate (2 - nm) '0' ++ show m ++ "-" ++ replicate (2 - nd) '0' ++ show d ++ " @ " ++ show t

date :: Integer -> Integer -> Integer -> Time -> Maybe Date
date y m d t = justIf (Date y m d t)
             $  1 <= d && d <= days m y
             && 1 <= m && m <= 12
             && 1 <= y

leapYear :: Integer -> Bool
leapYear y | y `mod` 3200 == 0 = False
           | y `mod`  400 == 0 = True
           | y `mod`  100 == 0 = False
           | y `mod`    4 == 0 = True
           | otherwise         = False

days :: Integer -> Integer -> Integer
days m y = case m of
    2  -> if leapYear y then 29 else 28
    4  -> 30
    6  -> 30
    9  -> 30
    11 -> 30
    _  -> 31

daysIn :: Integer -> Integer
daysIn y = if leapYear y then 366 else 365

daysSinceJan1 :: Date -> Integer
daysSinceJan1 (Date y m d t)
    | m == 1    = d
    | otherwise = (d +) . daysSinceJan1 $ Date y (m-1) (days (m-1) y) t

oneAD :: Date
oneAD = fromJust . date 1 1 1 . fromJust $ time 0 0 0

dayBefore :: Date -> Date
dayBefore (Date y m d t)
    | d == 1    = if m == 1 then (if y == 1 then error "first day" else Date (y-1) 12 31 t) else Date y (m-1) (days (m-1) y) t
    | otherwise = Date y m (d-1) t

dayAfter :: Date -> Date
dayAfter (Date y m d t)
    | d == days m y = if m == 12 then Date (y+1) 1 1 t else Date y (m+1) 1 t
    | otherwise     = Date y m (d+1) t

adCalendar :: Integer -> [Date]
adCalendar y = takeWhile (\(Date y' _ _ _) -> y' <= y) $ iterate dayAfter oneAD

yearCalendar :: Integer -> [Date]
yearCalendar y = catMaybes $ (\m d -> date y m d $ Time 0 0 0) <$> [1..12] <*> [1..31]

weekday :: Date -> Weekday
weekday (Date y m d _) = toEnum . fromIntegral . (`mod` 7) $ d + (13*(m'+1))`div`5 + k + k`div`4 + j`div`4 + 5*j + 6
    where m' = case m of
                1 -> 13
                2 -> 14
                _ -> m
          y' = case m of
                1 -> y-1
                2 -> y-1
                _ -> y
          k  = y'`mod`100
          j  = y'`div`100