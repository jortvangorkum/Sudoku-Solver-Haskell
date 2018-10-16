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
parseSudoku input = zipWith4 zip4 (parseInput input) possibleValuesArray columns rows
    where
    convertToInt :: String -> Int
    convertToInt (char:_) = digitToInt char
    parseInput :: String -> [[Int]]
    parseInput input = map (map convertToInt . words) (lines input)
    possibleValuesArray :: [[[Int]]]
    possibleValuesArray = (replicate 9 . replicate 9) [1..9]
    columns :: [[Int]]
    columns = replicate 9 [0..8]
    rows :: [[Int]]
    rows = transpose columns

printRow :: Row -> String
printRow [(v, _, _, _)] = show v
printRow ((v, _, x, y):xs)
    | (x+1) `mod` 3 == 0 = show v ++ " | " ++ printRow xs
    | otherwise          = show v ++ " "   ++ printRow xs

printSudoku :: Sudoku -> String
printSudoku [row] = printRow row
printSudoku (row@(field@(_, _, _, y):fields):rows)
    | (y+1) `mod` 3 == 0 = printRow row ++ printLine ++ printSudoku rows
    | otherwise          = printRow row ++ "\n" ++ printSudoku rows
    where
    printLine = "\n---------------------\n"

unconcatFields :: [Field] -> Sudoku
unconcatFields = groupWith (\(v, pVs, x, y) -> y)

{-
    Solving Sudoku
-}

replaceFields :: [Field] -> [Field] -> [Field]
replaceFields [] xs = xs
replaceFields _ []  = []
replaceFields rp@(rf@(_,_,rx,ry):rfs) (f@(_,_,fx,fy):fs)
    | rx == fx && ry == fy = rf : replaceFields rfs fs
    | otherwise            = f : replaceFields rp fs

emptyFields :: [Field] -> [Field]
emptyFields = filter (\(v, _, _, _) -> v == 0)

checkField :: Field -> [Field] -> Bool
checkField _ [] = True
checkField f@(v, _, x, y) (f2@(v2, _, x2, y2):xs)
    | check f f2 = False
    | otherwise  = checkField f xs
    where
    -- Check Row Column and Plane
    check :: Field -> Field -> Bool
    check (v, _, x, y) (v2, _, x2, y2) = v == v2 && ((x == x2) || (y == y2) || ((x`div`3) == (x2`div`3) && (y`div`3) == (y2`div`3)))

loopThroughpValues :: Field -> [Field] -> Field
loopThroughpValues (_, [], x, y) fields = (0, [1..9], x, y)
loopThroughpValues (v, pV:pVs, x, y) fields
    | checkField (pV, pVs, x, y) fields = (pV, pVs, x, y)
    | otherwise                         = loopThroughpValues (v, pVs, x, y) fields

solveEmptyFields :: [Field] -> [Field] -> [Field] -> [Field]
solveEmptyFields sudoku done [] = replaceFields done sudoku
solveEmptyFields sudoku done notdone@(x:xs) = case loopThroughpValues x (replaceFields done sudoku) of
    f@(0, _, _, _) -> solveEmptyFields sudoku (init done) (last done : f : xs)
    f              -> solveEmptyFields sudoku (done ++ [f]) xs

solveSudoku :: Sudoku -> Sudoku
solveSudoku sudoku = unconcatFields $ solveEmptyFields fields [] (emptyFields fields)
    where
    fields :: [Field]
    fields = concat sudoku

{-
    Main function
-}

main = interact solver where
    solver :: String -> String
    solver = printSudoku . solveSudoku . parseSudoku


{-
    Tests
-}

testSudoku :: [Field]
testSudoku = [(0,[1..9],0,0),(4,[1..9],1,0),(0,[1..9],2,0),(0,[1..9],3,0),(0,[1..9],4,0),(0,[1..9],5,0),(1,[1..9],6,0),(7,[1..9],7,0),(9,[1..9],8,0),
              (0,[1..9],0,1),(0,[1..9],1,1),(2,[1..9],2,1),(0,[1..9],3,1),(0,[1..9],4,1),(8,[1..9],5,1),(0,[1..9],6,1),(5,[1..9],7,1),(4,[1..9],8,1),
              (0,[1..9],0,2),(0,[1..9],1,2),(6,[1..9],2,2),(0,[1..9],3,2),(0,[1..9],4,2),(5,[1..9],5,2),(0,[1..9],6,2),(0,[1..9],7,2),(8,[1..9],8,2),
              (0,[1..9],0,3),(8,[1..9],1,3),(0,[1..9],2,3),(0,[1..9],3,3),(7,[1..9],4,3),(0,[1..9],5,3),(9,[1..9],6,3),(1,[1..9],7,3),(0,[1..9],8,3),
              (0,[1..9],0,4),(5,[1..9],1,4),(0,[1..9],2,4),(0,[1..9],3,4),(9,[1..9],4,4),(0,[1..9],5,4),(0,[1..9],6,4),(3,[1..9],7,4),(0,[1..9],8,4),
              (0,[1..9],0,5),(1,[1..9],1,5),(9,[1..9],2,5),(0,[1..9],3,5),(6,[1..9],4,5),(0,[1..9],5,5),(0,[1..9],6,5),(4,[1..9],7,5),(0,[1..9],8,5),
              (3,[1..9],0,6),(0,[1..9],1,6),(0,[1..9],2,6),(4,[1..9],3,6),(0,[1..9],4,6),(0,[1..9],5,6),(7,[1..9],6,6),(0,[1..9],7,6),(0,[1..9],8,6),
              (5,[1..9],0,7),(7,[1..9],1,7),(0,[1..9],2,7),(1,[1..9],3,7),(0,[1..9],4,7),(0,[1..9],5,7),(2,[1..9],6,7),(0,[1..9],7,7),(0,[1..9],8,7),
              (9,[1..9],0,8),(2,[1..9],1,8),(8,[1..9],2,8),(0,[1..9],3,8),(0,[1..9],4,8),(0,[1..9],5,8),(0,[1..9],6,8),(6,[1..9],7,8),(0,[1..9],8,8)]

tests = TestList [
    "test checkFieldH"                          ~: False                                ~=? checkField (1, [], 0, 0) testSudoku,
    "test checkFieldV"                          ~: False                                ~=? checkField (3, [], 0, 0) testSudoku,
    "test checkFieldP"                          ~: False                                ~=? checkField (2, [], 0, 0) testSudoku,
    "test checkFieldP"                          ~: True                                 ~=? checkField (8, [], 0, 0) testSudoku,
    "test replaceField (should replace)"        ~: [(2, [], 0, 0), (1, [1..9], 0, 1)]   ~=? replaceFields [(2, [], 0, 0)] [(1, [1..9], 0, 0), (1, [1..9], 0, 1)],
    "test replaceField (should not replace)"    ~: [(1, [1..9], 0, 0)]                  ~=? replaceFields [(2, [], 0, 1)] [(1, [1..9], 0, 0)],
    "test replaceField (should not replace)"    ~: [(1, [1..9], 0, 0)]                  ~=? replaceFields [(2, [], 1, 0)] [(1, [1..9], 0, 0)],
    "test emptyField (should return something)" ~: [(0, [], 0, 0)]                      ~=? emptyFields [(0, [], 0, 0), (1, [], 0, 1)],
    "test emptyField (should return something)" ~: []                                   ~=? emptyFields [(5, [], 0, 0), (1, [], 0, 1)],
    "test loopThroughpValues"                   ~: (8, [9], 0, 0)                       ~=? loopThroughpValues (0, [1..9], 0, 0) testSudoku,
    "test loopThroughpValues"                   ~: (3, [4..9], 2, 0)                    ~=? loopThroughpValues (0, [1..9], 2, 0) (replaceFields [(8, [9], 0, 0)] testSudoku)
    ]




