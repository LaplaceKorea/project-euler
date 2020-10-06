module Main where

import Data.Time (diffUTCTime)
import Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import Questions (q)
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
