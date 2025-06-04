{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Puzzle
-- Description : A domain-specific language for describing and solving constraint-based mathematical puzzles.
--
-- This module defines a monadic type class, 'Puzzle', for declaratively specifying 
-- constraint-based mathematical puzzles. Puzzles are modeled using integer variables 
-- and linear constraints over those variables.
--
-- The 'Puzzle' interface provides primitives to:
--
--   * create integer variables,
--   * specify linear restrictions (constraints) over those variables,
--   * query for solutions satisfying all given constraints.
--
-- A concrete implementation, 'CSPPuzzle', is included, which leverages the @csp@ 
-- library as a backend constraint solver.
--
-- This DSL is intended to describe and solve puzzles such as Sudoku, the eight 
-- queens puzzle, and other logic-based problems that can be expressed as a set of 
-- linear constraints over integer variables.
--
-- A typical use of the DSL involves writing a monadic puzzle description using
-- the 'Puzzle' class, returning a list of variables to be known; and then requesting 
-- either a single solution or all solutions that satisfy the constraints.
--

module Puzzle (Puzzle(..), CSPPuzzle, Var) where

import Control.Monad.CSP
    ( allCSPSolutions,
      constraint,
      constraint1,
      constraint2,
      mkDV,
      CSP,
      DV )
import LinearExpr (LinearExpr (..), LinExpr)
import Control.Monad.State (StateT (runStateT), MonadState (..), get, lift)
import Data.List (tails)

-- | A type class for defining and solving logical-mathematical puzzles using 
-- linear constraints over integer variables.
--
-- This class provides a monadic interface to:
--
--   * Declare integer variables with a given domain,
--   * Impose constraints on variables as comparisons between linear expressions,
--   * Query for one or all solutions satisfying the constraints.
--
-- The class is parameterized over:
--
--   * @m@: the monadic context in which variables are declared and constraints are added,
--   * @e@: the type of linear expressions,
--   * @v@: the type of variables.
--
-- The functional dependency @m -> e@ ensure that the expression type is uniquely
-- determined by the monadic context.
--
class (Monad m, LinearExpr e v) => Puzzle m e v | m -> e where
    --------------------------------------------------------------------------
    -- Variable creation
    --------------------------------------------------------------------------

    -- | Create a fresh variable with the given finite domain.
    newVar :: [Int] -> m v

    -- | Create multiple fresh variables, each with the same given domain.
    newVars :: Int -> [Int] -> m [v]
    newVars n dom = mapM (\_ -> newVar dom) [1..n]


    --------------------------------------------------------------------------
    -- Constraints
    --------------------------------------------------------------------------

    -- | A no-op representing an empty or trivially satisfied constraint.
    empty :: m ()

    -- | Assign a specific value to a variable
    setValue :: Int -> v -> m ()
    setValue n v = 1 *. v .=. constant n

    -- | Forbid a variable from taking a specific value.
    forbidValue :: Int -> v -> m ()
    forbidValue n v = 1 *. v ./=. constant n

    -- | Constrain two variables to have the same value.
    equalVars :: v -> v -> m ()
    equalVars v u = 1 *. v .=. 1 *. u

    -- | Constrain two variables to have different values.
    diffVars :: v -> v -> m ()
    diffVars v u = 1 *. v ./=. 1 *. u

    -- | Constrain all variables in the list to be equal.
    allEqual :: [v] -> m ()
    allEqual (v:u:vs) = equalVars v u >> allEqual (u:vs)
    allEqual _        = empty

    -- | Constrain all variables in the list to take different values.
    allDiff :: [v] -> m ()
    --allDiff vs = mapM_ (uncurry diffVars) [(x, y) | (x:ys) <- tails vs, y <- ys]
    allDiff vs = sequence_ [diffVars x y | (x:ys) <- tails vs, y <- ys]

    -- | Impose a constraint between two linear expressions using a predicate
    -- over their evaluated integer values.
    cmp :: (Int -> Int -> Bool) -> e -> e -> m ()

    -- | Constrain two linear expressions to be equal.
    (.=.) :: e -> e -> m ()
    (.=.) = cmp (==)

    -- | Constrain two linear expressions to be different.
    (./=.) :: e -> e -> m ()
    (./=.) = cmp (/=)

    -- | Constrain the left expression to be strictly less than the right.
    (.<.) :: e -> e -> m ()
    (.<.) = cmp (<)

    -- | Constrain the left expression to be less than or equal to the right.
    (.<=.) :: e -> e -> m ()
    (.<=.) = cmp (<=)

    -- | Constrain the left expression to be strictly greater than the right.
    (.>.) :: e -> e -> m ()
    (.>.) = cmp (>)

    -- | Constrain the left expression to be greater than or equal to the right.
    (.>=.) :: e -> e -> m ()
    (.>=.) = cmp (>=)

    infix 5 .=., ./=., .<., .<=., .>., .>=.

    --------------------------------------------------------------------------
    -- Solving
    --------------------------------------------------------------------------

    -- | Compute a value assignment to the given variables that satisfy the constraints 
    -- in the monad, if one exists.
    oneSol :: m [v] -> Maybe [Int]
    oneSol p = case allSol p of
        []    -> Nothing
        (s:_) -> Just s

    -- | Compute all value assignments to the given variables that satisfy the constraints 
    -- in the monad.
    allSol :: m [v] -> [[Int]]


-- | A representation of integer variables.
--
-- A variable consists of:
--
--   * A unique name, used to define a simple and safe ordering over variables,
--   * A 'DV' value from @Control.Monad.CSP@, which encodes the variable's domain
--     and serves as the internal representation used by the CSP solver to encode 
--     and solve restrictions.
data Var = Var {name :: String, dv :: DV [Int] Int}

instance Eq Var where
    (==) :: Var -> Var -> Bool
    v0 == v1 = name v0 == name v1

instance Ord Var where
    (<=) :: Var -> Var -> Bool
    v0 <= v1 = name v0 <= name v1


-- | A monad used for defining and solving puzzles in the DSL.
--
-- This is a concrete implementation of the 'Puzzle' class, using the
-- @Control.Monad.CSP@ library for constraint handling and solving.
--
-- The monad stacks:
--
--   * A 'StateT' layer used solely for generating fresh variable names,
--   * The 'CSP' monad, which maintains variable domains, enforces constraints,
--     and performs constraint solving.
--
-- Each new variable is assigned a unique internal name.
--

type CSPPuzzle = StateT Int (CSP [Int])

-- | An instance of the 'Puzzle' class using the @Control.Monad.CSP@ solver backend.
--
-- This implementation uses the 'CSP' monad to handle constraint logic, and a 'StateT'
-- layer to generate unique names for variables.

instance Puzzle CSPPuzzle (LinExpr Var) Var where

    newVar :: [Int] -> CSPPuzzle Var
    newVar dom = do
        n <- tick
        v <- lift (mkDV dom)
        return $ Var { name = show n, dv = v }

    empty :: CSPPuzzle ()
    empty = return ()

    setValue :: Int -> Var -> CSPPuzzle ()
    setValue n v = lift $ constraint1 (== n) (dv v)

    forbidValue :: Int -> Var -> CSPPuzzle ()
    forbidValue n v = lift $ constraint1 (/= n) (dv v)

    equalVars :: Var -> Var -> CSPPuzzle ()
    equalVars v0 v1 = lift $ constraint2 (==) (dv v0) (dv v1)

    diffVars :: Var -> Var -> CSPPuzzle ()
    diffVars v0 v1 = lift $ constraint2 (/=) (dv v0) (dv v1)

    cmp :: (Int -> Int -> Bool) -> LinExpr Var -> LinExpr Var -> CSPPuzzle ()
    cmp op e0 e1 =
        let
            (ts0, c0) = normalise e0
            (ts1, c1) = normalise e1
            n         = length ts0
            vars      = map (dv . fst) $ ts0 ++ ts1
            f vs      = foldr (+) c0 (zipWith (*) (map snd ts0) (take n vs))
                        `op`
                        foldr (+) c1 (zipWith (*) (map snd ts1) (drop n vs))
        in lift $ constraint f vars

    allSol :: CSPPuzzle [Var] -> [[Int]]
    allSol m = allCSPSolutions $ runStateT m 0 >>= \(r, _) -> return (map dv r)

tick :: Monad m => StateT Int m Int
tick = do
    n <- get
    put (n+1)
    return n
