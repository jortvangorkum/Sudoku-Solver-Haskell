import Data.Char
import Data.List
import Data.Maybe
import GHC.Exts

-- | Model
type Field  = (Int, [Int], Int, Int)
type Row    = [Field]
type Sudoku = [Row]  

index :: [Int] -> [Int]
index [] = []
index y@(x:xs) = index xs ++ [length y]

-- check aanroepen met "check field (concat sudoku)"
check :: Field -> [Field] -> Bool
check (v, pV, x, y) [] = True
check field@(v, pV, x, y) ((v2, pV2, x2, y2):xs)
    -- Check Row Column and Plane
    | v == v2 && ((x == x2) || (y == y2) || (x`div`3 == x2`div`3 && y==y2`div`3)) = False
    | otherwise = check field xs

unconcat :: [Field] -> [[Field]]
unconcat = groupWith (\(v, pV, x, y) -> y) 

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