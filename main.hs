module Main where

import Data.Char
import Data.List

type Value = Int
type PossibleValues = [Int]
type ColumnNr = Int
type RowNr = Int
type Field = (Value, PossibleValues, ColumnNr, RowNr)

type Row = [Field]
type Grid = [Row]

main :: IO()
main = interact solver

solver :: String -> String
solver input = unlines (map unlines (printGrid (parseInput input)))

{--
  Parsing Input
--}

parseInput :: String -> Grid
parseInput input = zipWith4 zip4 values possibleValues columnPositions rowPositions
  where
    getRowSize :: [[Int]] -> Int
    getRowSize (row:_) = length row
    getColumnSize :: [[Int]] -> Int
    getColumnSize array = length array

    values      = parseToIntGrid input
    rowSize     = getRowSize values
    columnSize  = getColumnSize values
    possibleValues  = possibleValuesGrid rowSize columnSize
    columnPositions = columnPositionsGrid rowSize columnSize
    rowPositions    = rowPositionsGrid rowSize columnSize

parseToIntGrid :: String -> [[Int]]
parseToIntGrid input = map (map toInt . words) (lines input)
  where
    toInt :: String -> Int
    toInt (char:_) = digitToInt char

possibleValuesGrid :: Int -> Int -> [[[Int]]]
possibleValuesGrid width height = (replicate height . replicate width) [1..9]

columnPositionsGrid :: Int -> Int -> [[Int]]
columnPositionsGrid width height = replicate height [0..width]

rowPositionsGrid :: Int -> Int -> [[Int]]
rowPositionsGrid width height = transpose $ columnPositionsGrid width height

{--
  Printing Grid
--}

printGrid :: Grid -> [[String]]
printGrid grid = map (map (show . \(value, _, _, _) -> value)) grid