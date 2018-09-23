module Main where

import Data.Char
import Data.List
import Data.Maybe

-- | Model
-- Field: value, possibleValues, rowIndex, columnIndex
type Field  = (Int, [Int], Int, Int)
type Row    = [Field]
type Sudoku = [Row]    

main = interact solver where
    solver :: String -> String
    solver input = printSudoku (solveSudoku (parseSudoku input))
        where
        parseSudoku :: String -> [[Field]]
        parseSudoku input = zipWith4 f (parseInput input) (possibleValuesArray (parseInput input)) (map index (parseInput input)) (transpose (map index (parseInput input)))
            where  
            convert :: String -> Int
            convert (char:_) = digitToInt char
            parseInput :: String -> [[Int]]
            parseInput input = map (map convert . words) (lines input)
            possibleValues :: Int -> [Int]
            possibleValues x 
                | x == 0 = [1..9]
                | otherwise = []
            possibleValuesArray :: [[Int]] -> [[[Int]]]
            possibleValuesArray = map (map possibleValues)
            index :: [Int] -> [Int]
            index [] = []
            index y@(x:xs) = index xs ++ [length y]
            f :: [Int] -> [[Int]] -> [Int] -> [Int] -> [(Int, [Int], Int, Int)]
            f = zipWith4 (\x y v z -> (x,y,z,v))
        printSudoku :: Sudoku -> String
        printSudoku table = unlines (map (intercalate "|" . map printField) table) 
            where
            printField :: Field -> String
            printField (value, pValues, x, y) = [intToDigit value]
        solveSudoku :: Sudoku -> Sudoku
        solveSudoku sudoku = sudoku
            where 
            solve :: Field -> Field
            solve = undefined
                where
                possibleValues :: [Int]
                possibleValues [] = undefined
                possibleValues values@(x:xs) = undefined
        