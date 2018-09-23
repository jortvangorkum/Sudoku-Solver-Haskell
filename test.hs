import Data.Char
import Data.List
import Data.Maybe

-- | Model
type Field  = (Int, [Int], Int, Int)
type Row    = [Field]
type Sudoku = [Row]  

check :: Int -> [Int] -> [Int] -> Bool
check value row column = elem getRow row && elem getColumn column
    where
    getRow = undefined
    getColumn = undefined

index :: [Int] -> [Int]
index [] = []
index y@(x:xs) = index xs ++ [length y]

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
    index y@(x:xs) = index xs ++ [length y - 1] 
    f :: [Int] -> [[Int]] -> [Int] -> [Int] -> [(Int, [Int], Int, Int)]
    f = zipWith4 (\x y v z -> (x,y,z,v))