module Main where

import Data.Char
import Data.List
import GHC.Exts
import Debug.Trace


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
        solveSudoku sudoku = unconcat (solve [] (concat sudoku))
            where 
            unconcat :: [Field] -> [[Field]]
            unconcat = groupWith (\(v, pVs, x, y) -> y) 
            solve :: [Field] -> [Field] -> [Field]
            solve done [] = done
            solve done notdone@(x:xs)
                | possibleValues x (done ++ notdone) = solve (done ++ [x]) xs
                | otherwise = solve (init done) (map (\(v, pVs, x, y) -> (v, [0..9], x, y)) (last done : notdone)) 
                where
                possibleValues :: Field -> [Field] -> Bool
                possibleValues (v, [], x, y) fields = False
                possibleValues (v, pV:pVs, x, y) fields
                    | check (pV, pVs, x, y) fields = True
                    | otherwise = possibleValues (v, pVs, x, y) fields
                    where
                    check :: Field -> [Field] -> Bool
                    check (v, _, x, y) [] = True
                    check field@(v, _, x, y) ((v2, _, x2, y2):xs)
                        -- Check Row Column and Plane
                        | v == v2 && ((x == x2) || (y == y2) || (x`div`3 == x2`div`3 && y`div`3==y2`div`3)) = False
                        | otherwise = trace ("v:" ++ show v ++ " x:" ++ show x ++ " y:" ++ show y) $ check field xs
        