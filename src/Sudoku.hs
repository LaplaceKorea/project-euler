module Sudoku where

import Control.Applicative (Alternative ((<|>)))
import Control.Lens ((^.))
import Data.Foldable.Toolbox (maximumOf, minimumOf)
import Data.List.Toolbox (chunksOf, intercalate, (\\))
import Data.Map (Map, (!))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import FunctionExtra (twoDimListToMap)
import Linear

type Sudoku = Map (V2 Int) Int

sudoku :: [String] -> Sudoku
sudoku = twoDimListToMap (read . pure) 0 0 Map.empty

box, row, col :: V2 Int -> Sudoku -> [Int]
box (V2 x y) m = filter (/= 0) . map (m !) $ V2 <$> [x' .. x' + 2] <*> [y' .. y' + 2]
  where
    x', y' :: Int
    x' = 3 * (x `div` 3)
    y' = 3 * (y `div` 3)
row (V2 _ y) m = filter (/= 0) . map (m !) $ V2 <$> [0 .. 8] <*> [y]
col (V2 x _) m = filter (/= 0) . map (m !) $ V2 <$> [x] <*> [0 .. 8]

options :: V2 Int -> Sudoku -> [Int]
options v s = case s ! v of
    0 -> [1 .. 9] \\ (Set.toList . Set.fromList) (box v s ++ row v s ++ col v s)
    _ -> []

solve' :: V2 Int -> Sudoku -> Maybe Sudoku
solve' (V2 9 y) s = solve' (V2 0 (y + 1)) s
solve' (V2 _ 9) s = Just s
solve' v@(V2 x y) s = case s ! v of
    0 -> solve'' v s (options v s)
    _ -> solve' (V2 (x + 1) y) s
  where
    solve'' :: V2 Int -> Sudoku -> [Int] -> Maybe Sudoku
    solve'' _ _ [] = Nothing
    solve'' v@(V2 x y) s (k : ks) = solve' (V2 (x + 1) y) (Map.insert v k s) <|> solve'' v s ks

solve :: Sudoku -> Maybe Sudoku
solve = solve' $ V2 0 0

displayMap :: (Maybe a -> String) -> Map (V2 Int) a -> [String]
displayMap f m =
    let rmin = fromMaybe 0 . minimumOf (^. _x) $ Map.keys m
        rmax = fromMaybe 0 . maximumOf (^. _x) $ Map.keys m
        cmin = fromMaybe 0 . minimumOf (^. _y) $ Map.keys m
        cmax = fromMaybe 0 . maximumOf (^. _y) $ Map.keys m
     in chunksOf (cmax - cmin + 1) . concat . Map.elems $ foldr (\k g -> Map.insert k (f $ m Map.!? k) g) Map.empty $ V2 <$> [rmin .. rmax] <*> [cmin .. cmax]

displaySudoku :: Maybe Sudoku -> [String]
displaySudoku = maybe [""] $ intercalate ["---+---+---"] . chunksOf 3 . map (intercalate "|" . chunksOf 3) . displayMap (maybe "" show)
