module Sudoku (playSudoku) where

import Puzzle
import Data.Maybe ( fromMaybe, isNothing )

playSudoku :: IO ()
playSudoku = do
    -- Parse grid
    putStrLn "Reading sudoku grid from file sudoku.txt"
    contents <- readFile "app/sudoku.txt"
    let grid = parseSudokuGrid contents
    -- Solve puzzle
    putStrLn "Solving sudoku grid ..."
    case allSol (sudoku grid) of
        []      -> putStrLn "Malformed sudoku grid"
        [sol]   -> putStrLn "Solution:" >> prettyPrintSudoku sol
        (sol:_) -> putStrLn "This sudoku allows more than one solution! One solution is:" >> prettyPrintSudoku sol

-- The Sudoku puzzle
sudoku :: [Maybe Int] -> CSPPuzzle [Var]
sudoku grid = do
    -- Create a variable with singleton or full domain according to cell data.
    vs <- mapM (\val -> newVar (if isNothing val then [1..9] else [fromMaybe 0 val])) grid
    -- Ensure that elements in each row, column, and subgrid are all different.
    mapM_ allDiff $ rows vs ++ cols vs ++ subg vs
    -- Return all variables
    return vs

-- Divides a flat version of a sudoku grid into rows, columns and 3x3 subgrids
rows, cols, subg :: [a] -> [[a]]
rows xs = [[xs!!j | j <- [0..80], j < 9*(i+1), j >= 9*i] | i<-[0..8]]
cols xs = [[xs!!j | j <- [0..80], mod j 9 == i]  | i<-[0..8]]
subg xs = [[xs!!k | k <- [0..80], mod k 9 `elem` [i,i+1,i+2], k < (j+1)*3*9, k >= j*3*9 ] | i <- [0,3,6], j <- [0,1,2]]

-- Parses a sudoku grid
parseSudokuGrid :: String -> [Maybe Int]
parseSudokuGrid input = map parseChar $ concat $ lines input
  where
    parseChar '.' = Nothing
    parseChar c   = Just (read [c])

-- Pretty prints a sudoku solution
prettyPrintSudoku :: [Int] -> IO ()
prettyPrintSudoku [] = return ()
prettyPrintSudoku xs = printRow (take 9 xs) >> prettyPrintSudoku (drop 9 xs)
    where
        printRow vs = mapM_ (\v -> putStr (show v) >> putStr " ") vs >> putStrLn ""
