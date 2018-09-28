module Main where

import Data.Char
import Data.List
import GHC.Exts
import Debug.Trace

-- | Model
-- Field: value, possibleValues, columnIndex, rowIndex
type Field  = (Int, [Int], Int, Int)
type Row    = [Field]
type Sudoku = [Row] 
     
{-
    Parsing and printing sudoku
-}

parseSudoku :: String -> [[Field]]
parseSudoku input = zipWith4 f (parseInput input) (possibleValuesArray (parseInput input)) (map index (parseInput input)) (transpose (map index (parseInput input)))
    where  
    convert :: String -> Int
    convert (char:_) = digitToInt char
    parseInput :: String -> [[Int]]
    parseInput input = map (map convert . words) (lines input)
    possibleValues :: Int -> [Int]
    possibleValues x = [1..9]
    possibleValuesArray :: [[Int]] -> [[[Int]]]
    possibleValuesArray = map (map possibleValues)
    index :: [Int] -> [Int]
    index [] = []
    index y@(x:xs) = index xs ++ [length y - 1]
    f :: [Int] -> [[Int]] -> [Int] -> [Int] -> [(Int, [Int], Int, Int)]
    f = zipWith4 (\x y v z -> (x,y,z,v))

printSudoku :: Sudoku -> String
printSudoku table = unlines (map (intercalate "|" . map printField) table) 
    where
    printField :: Field -> String
    printField (value, pValues, x, y) = [intToDigit value]

showField :: Field -> String
showField (v, pVs, x, y) = "(v:" ++ show v ++ " pVs:" ++ show pVs ++ " x:" ++ show x ++ " y:" ++ show y ++ ")"

{-
    Solving Sudoku
-}

getColumnIndex :: Field -> Int
getColumnIndex (_,_,x,_) = x

getRowIndex :: Field -> Int
getRowIndex (_,_,_,y) = y

getField :: Int -> Int -> [Field] -> Field
getField x y [] = error "Field is not found"
getField x y (field@(_,_,fx, fy):fs)
    | x == fx && y == fy = field
    | otherwise = getField x y fs

setField :: Field -> [Field] -> [Field]
setField _ [] = []
setField f@(_, _, x, y) (f2@(_, _, fx, fy):fs)
    | x == fx && y == fy = f : setField f fs
    | otherwise = f2 : setField f fs

replaceFields :: [Field] -> [Field] -> [Field]
replaceFields _ [] = []
replaceFields [] _ = []
replaceFields replace@(rf@(_,_,rx,ry):rfs) fields@(f@(_,_,fx,fy):fs)
    | rx == fx && ry == fy = rf : replaceFields rfs fs
    | otherwise = f : replaceFields replace fs

emptyFields :: [Field] -> [Field]
emptyFields = filter (\(v, _, _, _) -> v == 0)

checkField :: Field -> [Field] -> Bool
checkField (v, _, x, y) [] = True
checkField field@(v, _, x, y) ((v2, _, x2, y2):xs)
    -- Check Row Column and Plane
    | v == v2 && ((x == x2) || (y == y2) || (x`div`3 == x2`div`3 && y`div`3==y2`div`3)) = False
    | otherwise = checkField field xs

loopThroughpValues :: Field -> [Field] -> Field
loopThroughpValues field@(_, [], _, _) fields = field
loopThroughpValues (v, pV:pVs, x, y) fields
    | checkField (pV, pVs, x, y) fields = (pV, pVs, x, y)
    | otherwise = loopThroughpValues (v, pVs, x, y) fields

solveEmptyFields :: [Field] -> [Field] -> [Field] -> [Field]
solveEmptyFields sudoku done [] = done
solveEmptyFields sudoku done notdone@(x:xs) = case loopThroughpValues x sudoku of 
    field@(_, [], x, y)  -> trace1 field done $ solveEmptyFields (setField (0, [1..9], getColumnIndex (last done), getRowIndex (last done)) sudoku) (init done) (map resetField ((last done) : notdone))
    field@(v, pVs, x, y) -> trace2 field done $ solveEmptyFields (setField field sudoku) (done ++ [field]) xs
    where 
    resetField :: Field -> Field
    resetField (v, _, x, y) = (v, [1..9], x, y)
    trace1 field done = trace ("V1:" ++ showField field ++ " Done1:" ++ show (length done))
    trace2 field done = trace ("V2:" ++ showField field ++ " Done2:" ++ show (length done))

solveSudoku :: Sudoku -> Sudoku
solveSudoku sudoku = unconcat  (solveEmptyFields fields [] (emptyFields fields))
    where 
    unconcat :: [Field] -> [[Field]]
    unconcat = groupWith (\(v, pVs, x, y) -> y)
    fields :: [Field]
    fields = concat sudoku

{-
    Main function
-}

main = interact solver where
    solver :: String -> String
    solver input = printSudoku (solveSudoku (parseSudoku input))


        
        
        
        