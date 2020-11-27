module Main where

import Data.Time (diffUTCTime)
import Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import Questions
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
    args <- getArgs
    let k = if null args then 1 else read $ head args
    mapM_
        ( \k -> do
            putStr $ "Q " ++ replicate (4 - length (show k)) ' ' ++ show k ++ ": "
            t1 <- systemToUTCTime <$> getSystemTime
            hFlush stdout
            qk <- q k
            putStr $ show qk ++ replicate (20 - length (show qk)) ' ' ++ " ("
            hFlush stdout
            t2 <- systemToUTCTime <$> getSystemTime
            let t = diffUTCTime t2 t1
            putStr $ show t ++ ")\n"
            hFlush stdout
        )
        [k .. 700]

-- | is there a better way to do this...?
q :: Integer -> IO Integer
q = \case
    1 -> q001
    2 -> q002
    3 -> q003
    4 -> q004
    5 -> q005
    6 -> q006
    7 -> q007
    8 -> q008
    9 -> q009
    10 -> q010
    11 -> q011
    12 -> q012
    13 -> q013
    14 -> q014
    15 -> q015
    16 -> q016
    17 -> q017
    18 -> q018
    19 -> q019
    20 -> q020
    21 -> q021
    22 -> q022
    23 -> q023
    24 -> q024
    25 -> q025
    26 -> q026
    27 -> q027
    28 -> q028
    29 -> q029
    30 -> q030
    31 -> q031
    32 -> q032
    33 -> q033
    34 -> q034
    35 -> q035
    36 -> q036
    37 -> q037
    38 -> q038
    39 -> q039
    40 -> q040
    41 -> q041
    42 -> q042
    43 -> q043
    44 -> q044
    45 -> q045
    46 -> q046
    47 -> q047
    48 -> q048
    49 -> q049
    50 -> q050
    51 -> q051
    52 -> q052
    53 -> q053
    54 -> q054
    55 -> q055
    56 -> q056
    57 -> q057
    58 -> q058
    59 -> q059
    60 -> q060
    61 -> q061
    62 -> q062
    63 -> q063
    64 -> q064
    65 -> q065
    66 -> q066
    67 -> q067
    68 -> q068
    69 -> q069
    70 -> q070
    71 -> q071
    72 -> q072
    73 -> q073
    74 -> q074
    75 -> q075
    76 -> q076
    77 -> q077
    78 -> q078
    79 -> q079
    80 -> q080
    90 -> q090
    92 -> q092
    96 -> q096
    99 -> q099
    _ -> error "that's it so far"