module PuzzleSpec (spec) where

import Test.Hspec
import Puzzle
import LinearExpr

spec :: Spec
spec = do

  describe "Handles single variable creation correctly" $ do

    it "creates a variable with a given, non-empty domain" $ do
      let puzzle :: CSPPuzzle [Var]
          puzzle = do
            x <- newVar [2..3]
            return [x]
      checkSolutions puzzle [[2],[3]]

    it "creates a variable with empty domain" $ do
      let puzzle :: CSPPuzzle [Var]
          puzzle = do
            x <- newVar []
            return [x]
      checkSolutions puzzle []

  describe "Handles multiple variable creations correctly" $ do

    it "creates several variables with a given, non-empty domain" $ do
      let puzzle :: CSPPuzzle [Var]
          puzzle = newVars 3 [2..3]
      checkSolutions puzzle [[2,2,2],[2,2,3],[2,3,2],[3,2,2],[3,3,2],[3,2,3],[2,3,3],[3,3,3]]

    it "creates several variables with empty domain" $ do
      let puzzle :: CSPPuzzle [Var]
          puzzle = newVars 3 []
      checkSolutions puzzle []

  describe "Handles empty restriction correctly" $ do

    it "Ignores empty restriction" $ do
      let puzzle :: CSPPuzzle [Var]
          puzzle = do
            n <- newVar [1..3]
            m <- newVar [9,10]
            empty
            return [n, m]
      checkSolutions puzzle [[1,9],[1,10],[2,9],[2,10],[3,9],[3,10]]

  describe "Handles value assignment and restriction correctly" $ do

    it "assigns a value in domain" $ do
      let puzzle :: CSPPuzzle [Var]
          puzzle = do
            n <- newVar [1..10]
            setValue 5 n
            return [n]
      checkSolutions puzzle [[5]]

    it "cannot assign a value not in domain" $ do
      let puzzle :: CSPPuzzle [Var]
          puzzle = do
            n <- newVar [1..10]
            setValue 15 n
            return [n]
      checkSolutions puzzle []

    it "forbids a value in domain" $ do
      let puzzle :: CSPPuzzle [Var]
          puzzle = do
            n <- newVar [1..6]
            forbidValue 5 n
            return [n]
      checkSolutions puzzle [[1],[2],[3],[4],[6]]

    it "ignores forbidden value if not in domain" $ do
      let puzzle :: CSPPuzzle [Var]
          puzzle = do
            n <- newVar [1..6]
            forbidValue 15 n
            return [n]
      checkSolutions puzzle [[1],[2],[3],[4],[5],[6]]

  describe "Handles variable (in)equality correctly" $ do

    it "assings the same value to a pair of variables" $ do
      let puzzle :: CSPPuzzle [Var]
          puzzle = do
            n <- newVar [1..10]
            m <- newVar [9..19]
            equalVars n m
            return [n, m]
      checkSolutions puzzle [[9,9],[10,10]]

    it "assings different values to a pair of variables" $ do
      let puzzle :: CSPPuzzle [Var]
          puzzle = do
            n <- newVar [8..10]
            m <- newVar [9..11]
            diffVars n m
            return [n, m]
      checkSolutions puzzle [[8,9],[8,10],[8,11],[9,10],[9,11],[10,9],[10,11]]

    it "does not find an answer if variables with disjoint domain must have the same value" $ do
      let puzzle :: CSPPuzzle [Var]
          puzzle = do
            n <- newVar [1,3,5,7]
            m <- newVar [2,4,6,8]
            equalVars n m
            return [n, m]
      checkSolutions puzzle []

    it "does not find an answer if variables with same singleton domain must have different values" $ do
      let puzzle :: CSPPuzzle [Var]
          puzzle = do
            n <- newVar [4]
            m <- newVar [4]
            diffVars n m
            return [n, m]
      checkSolutions puzzle []

    it "assings the same value to a list of variables" $ do
      let puzzle :: CSPPuzzle [Var]
          puzzle = do
            vs <- newVars 4 [1..5]
            allEqual vs
            return vs
      checkSolutions puzzle [[1,1,1,1], [2,2,2,2], [3,3,3,3], [4,4,4,4], [5,5,5,5]]

    it "assings different values to a list of variables" $ do
      let puzzle :: CSPPuzzle [Var]
          puzzle = do
            vs <- newVars 3 [1..3]
            allDiff vs
            return vs
      checkSolutions puzzle [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]

  describe "Handles linear expression comparisons correctly" $ do

    it "equality" $ do
      let puzzle :: CSPPuzzle [Var]
          puzzle = do
            x <- newVar [8..14]
            y <- newVar [1..10]
            3 *. x .=. 1 *. x <+> 4 *. y <+> constant 2
            return [x, y]
      checkSolutions puzzle [[9,4],[11,5],[13,6]]

    it "inequality" $ do
      let puzzle :: CSPPuzzle [Var]
          puzzle = do
            x <- newVar [3..5]
            y <- newVar [1,2]
            3 *. x ./=. 1 *. x <+> 4 *. y <+> constant 2
            return [x, y]
      checkSolutions puzzle [[4,1],[5,1],[3,2],[4,2]]

    it "lower than" $ do
      let puzzle :: CSPPuzzle [Var]
          puzzle = do
            x <- newVar [2..4]
            y <- newVar [1..3]
            z <- newVar [8..12]
            3 *. x .<. 1 *. z <+> 2 *. x  <+> (-2) *. y <+> constant (-5)
            return [x, y, z]
      checkSolutions puzzle [[2,1,10],[2,1,11],[2,1,12],[2,2,12],[3,1,11],[3,1,12],[4,1,12]]

    it "lower than or equal" $ do
      let puzzle :: CSPPuzzle [Var]
          puzzle = do
            x <- newVar [2..4]
            y <- newVar [1..3]
            z <- newVar [8..12]
            3 *. x .<=. 1 *. z <+> 2 *. x  <+> (-2) *. y <+> constant (-5)
            return [x, y, z]
      checkSolutions puzzle [[2,1,9],[2,1,10],[2,1,11],[2,1,12],[2,2,11],[2,2,12],[3,1,10],[3,1,11],[3,1,12],[4,1,11],[4,1,12],[3,2,12]]

    it "greater than" $ do
      let puzzle :: CSPPuzzle [Var]
          puzzle = do
            x <- newVar [2..8]
            y <- newVar [2..8]
            3 *. x .>. 2 *. x  <+> 2 *. y <+> constant 1
            return [x, y]
      checkSolutions puzzle [[6,2],[7,2],[8,2],[8,3]]

    it "greater than or equal" $ do
      let puzzle :: CSPPuzzle [Var]
          puzzle = do
            x <- newVar [2..8]
            y <- newVar [2..8]
            3 *. x .>=. 2 *. x  <+> 2 *. y <+> constant 1
            return [x, y]
      checkSolutions puzzle [[5,2],[6,2],[7,2],[8,2],[7,3],[8,3]]

    it "custom comparison" $ do
      let puzzle :: CSPPuzzle [Var]
          puzzle = do
            x <- newVar [2..7]
            y <- newVar [2..7]
            cmp (\a b -> b == a+2 || b == a-2) (1 *. x) (1 *. y)
            return [x, y]
      checkSolutions puzzle [[2,4],[3,5],[4,2],[4,6],[5,3],[5,7],[6,4],[7,5]]


checkSolutions :: Puzzle m e v => m [v] -> [[Int]] -> IO ()
checkSolutions puzzle sols = do
  allSol puzzle `shouldMatchList` sols
  oneSol puzzle `shouldBeIn` sols

shouldBeIn :: Maybe [Int] -> [[Int]] -> Expectation
shouldBeIn sol []    = sol `shouldBe` Nothing
shouldBeIn sol expec = shouldSatisfy sol (\r -> r `elem` map Just expec)
