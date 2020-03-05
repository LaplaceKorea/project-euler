module Main where

import NumbersExtra (digits)
import Questions
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    let k = if null args then 1 else read $ head args
    mapM_ (\n -> putStrLn $ "Question" ++ replicate (4 - length (digits n)) ' ' ++ show n ++ ": " ++ show (q n)) [k..700]
