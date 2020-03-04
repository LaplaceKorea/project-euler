module Main where

import NumbersExtra (digits)
import Questions

main :: IO ()
main = mapM_ (\n -> putStrLn $ "Question" ++ replicate (4 - length (digits n)) ' ' ++ show n ++ ": " ++ show (q n)) [1..700]
