module Queens (playQueens) where

import Puzzle
import LinearExpr
import Control.Monad (replicateM_)

playQueens :: IO ()
playQueens = do
    putStrLn "Choose number of queens"
    n <- read <$> getLine
    case oneSol (queens n) of
        Nothing  -> putStrLn $ "No solution exists for " ++ show n ++ " queens."
        Just sol -> putStrLn "One solution:" >> prettyPrintNQueens n sol

-- The n queens puzzle
queens :: Int -> CSPPuzzle [Var]
queens n = do
    -- Different columns
    vs <- newVars n [0..n-1]
    -- Different rows
    allDiff vs
    -- Different diagonals
    sequence_ [1 *. (vs!!i) <-> 1 *. (vs!!j) ./=. constant (j - i) | i <- [0..n-1], j <- [i+1..n-1]]
    sequence_ [1 *. (vs!!j) <-> 1 *. (vs!!i) ./=. constant (j - i) | i <- [0..n-1], j <- [i+1..n-1]]
    -- Return all variables
    return vs

-- Pretty prints (the transpose of) an n queens solution
prettyPrintNQueens :: Int -> [Int] -> IO ()
prettyPrintNQueens n = mapM_ (printCol n)
    where
        printCol n val = do
            replicateM_ val (putStr ". ")
            putStr "Q " 
            replicateM_ (n-val-1) (putStr ". ")
            putStrLn ""
