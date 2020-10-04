module Main where

import NumbersExtra (digits)
import Questions
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    let k = if null args then 1 else read $ head args
    mapM_ (\k -> do
        qk <- q k
        putStrLn $ "Q " ++ replicate (4 - length (digits k)) ' ' ++ show k ++ ": " ++ show qk
      ) [k..700]
