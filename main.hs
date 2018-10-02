module Main where

import           Data.Char
import           Data.List
import           Debug.Trace
import           GHC.Exts
import           Test.HUnit

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
    possibleValues _ = [1..9]
    possibleValuesArray :: [[Int]] -> [[[Int]]]
    possibleValuesArray = map (map possibleValues)
    index :: [Int] -> [Int]
    index [] = []
    index xs = index' xs []
        where
        index' [] acc       = acc
        index' y@(_:xs) acc = index' xs ((length y - 1) : acc)
    f :: [Int] -> [[Int]] -> [Int] -> [Int] -> [(Int, [Int], Int, Int)]
    f = zipWith4 (\x y v z -> (x,y,v,z))

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

resetField :: Field -> Field
resetField (v, [], x, y) = (v, [1..9], x, y)

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
replaceFields [] [] = []
replaceFields _ [] = []
replaceFields [] xs  = [] ++ xs
replaceFields replace@(rf@(_,_,rx,ry):rfs) fields@(f@(_,_,fx,fy):fs)
    | rx == fx && ry == fy = rf : replaceFields rfs fs
    | otherwise = f : replaceFields replace fs

emptyFields :: [Field] -> [Field]
emptyFields = filter (\(v, _, _, _) -> v == 0)

checkField :: Field -> [Field] -> Bool
checkField _ [] = True
checkField field@(v, _, x, y) ((v2, _, x2, y2):xs)
    -- Check Row Column and Plane
    | v == v2 && ((x == x2) || (y == y2) || ((x`div`3) == (x2`div`3) && (y`div`3) == (y2`div`3))) = False
    | otherwise = checkField field xs

loopThroughpValues :: Field -> [Field] -> Field
loopThroughpValues field@(_, [], _, _) fields = field
loopThroughpValues (v, pV:pVs, x, y) fields
    | checkField (pV, pVs, x, y) fields = (pV, pVs, x, y)
    | otherwise = loopThroughpValues (v, pVs, x, y) fields

solveEmptyFields :: [Field] -> [Field] -> [Field] -> [Field]
solveEmptyFields sudoku done [] = replaceFields done sudoku
solveEmptyFields sudoku done notdone@(x:xs) = case loopThroughpValues x (replaceFields done sudoku) of
    field@(_, [], x, y)  -> trace1 field done notdone $ solveEmptyFields sudoku (init done) (last done : resetField field : xs)
    field@(v, pVs, x, y) -> trace2 field done notdone $ solveEmptyFields sudoku (done ++ [field]) xs
    where
    trace1 field done (notdone:(notdone2:_)) = trace ("V1:" ++ showField field ++ " NotDone1:" ++ showField notdone ++ " NotDone1:" ++ showField notdone2)
    trace2 field done (notdone:(notdone2:_)) = trace ("V2:" ++ showField field ++ " NotDone2:" ++ showField notdone ++ " NotDone2:" ++ showField notdone2)

solveSudoku :: Sudoku -> Sudoku
solveSudoku sudoku = unconcat (solveEmptyFields fields [] (emptyFields fields))
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


{-
    Tests
-}

tests = TestList [
    "test resetField"                           ~: (0, [1..9], 0, 0)                    ~=? resetField (0, [], 0, 0),
    "test checkFieldH"                          ~: False                                ~=? checkField (1, [], 0, 0) sudoku,
    "test checkFieldV"                          ~: False                                ~=? checkField (3, [], 0, 0) sudoku,
    "test checkFieldP"                          ~: False                                ~=? checkField (2, [], 0, 0) sudoku,
    "test checkFieldP"                          ~: True                                 ~=? checkField (8, [], 0, 0) sudoku,
    "test replaceField (should replace)"        ~: [(2, [], 0, 0), (1, [1..9], 0, 1)]   ~=? replaceFields [(2, [], 0, 0)] [(1, [1..9], 0, 0), (1, [1..9], 0, 1)],
    "test replaceField (should not replace)"    ~: [(1, [1..9], 0, 0)]                  ~=? replaceFields [(2, [], 0, 1)] [(1, [1..9], 0, 0)],
    "test replaceField (should not replace)"    ~: [(1, [1..9], 0, 0)]                  ~=? replaceFields [(2, [], 1, 0)] [(1, [1..9], 0, 0)],
    "test emptyField (should return something)" ~: [(0, [], 0, 0)]                      ~=? emptyFields [(0, [], 0, 0), (1, [], 0, 1)],
    "test emptyField (should return something)" ~: []                                   ~=? emptyFields [(5, [], 0, 0), (1, [], 0, 1)],
    "test loopThroughpValues"                   ~: (8, [9], 0, 0)                       ~=? loopThroughpValues (0, [1..9], 0, 0) sudoku,
    "test loopThroughpValues"                   ~: (3, [4..9], 2, 0)                    ~=? loopThroughpValues (0, [1..9], 2, 0) (replaceFields [(8, [9], 0, 0)] sudoku)
    ]
    where
    sudoku = [(0,[1..9],0,0),(4,[1..9],1,0),(0,[1..9],2,0),(0,[1..9],3,0),(0,[1..9],4,0),(0,[1..9],5,0),(1,[1..9],6,0),(7,[1..9],7,0),(9,[1..9],8,0),
              (0,[1..9],0,1),(0,[1..9],1,1),(2,[1..9],2,1),(0,[1..9],3,1),(0,[1..9],4,1),(8,[1..9],5,1),(0,[1..9],6,1),(5,[1..9],7,1),(4,[1..9],8,1),
              (0,[1..9],0,2),(0,[1..9],1,2),(6,[1..9],2,2),(0,[1..9],3,2),(0,[1..9],4,2),(5,[1..9],5,2),(0,[1..9],6,2),(0,[1..9],7,2),(8,[1..9],8,2),
              (0,[1..9],0,3),(8,[1..9],1,3),(0,[1..9],2,3),(0,[1..9],3,3),(7,[1..9],4,3),(0,[1..9],5,3),(9,[1..9],6,3),(1,[1..9],7,3),(0,[1..9],8,3),
              (0,[1..9],0,4),(5,[1..9],1,4),(0,[1..9],2,4),(0,[1..9],3,4),(9,[1..9],4,4),(0,[1..9],5,4),(0,[1..9],6,4),(3,[1..9],7,4),(0,[1..9],8,4),
              (0,[1..9],0,5),(1,[1..9],1,5),(9,[1..9],2,5),(0,[1..9],3,5),(6,[1..9],4,5),(0,[1..9],5,5),(0,[1..9],6,5),(4,[1..9],7,5),(0,[1..9],8,5),
              (3,[1..9],0,6),(0,[1..9],1,6),(0,[1..9],2,6),(4,[1..9],3,6),(0,[1..9],4,6),(0,[1..9],5,6),(7,[1..9],6,6),(0,[1..9],7,6),(0,[1..9],8,6),
              (5,[1..9],0,7),(7,[1..9],1,7),(0,[1..9],2,7),(1,[1..9],3,7),(0,[1..9],4,7),(0,[1..9],5,7),(2,[1..9],6,7),(0,[1..9],7,7),(0,[1..9],8,7),
              (9,[1..9],0,8),(2,[1..9],1,8),(8,[1..9],2,8),(0,[1..9],3,8),(0,[1..9],4,8),(0,[1..9],5,8),(0,[1..9],6,8),(6,[1..9],7,8),(0,[1..9],8,8)]




