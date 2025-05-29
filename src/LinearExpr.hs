{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

-- |
-- Module      : HPuzzle.LinearExpr
-- Description : A small domain-specific language for linear expressions over integer coefficients.
--
-- This module defines a type class 'LinearExpr' for building and manipulating
-- linear expressions over integer coefficients. It also includes a concrete 
-- implementation 'LinExpr'.

module LinearExpr (
      NormForm
    , LinearExpr(..)
    , LinExpr
) where

import qualified Data.Map as Map (empty, insertWith, toList)

-- | Type for representing linear expressions over integer coefficients in normal form, 
-- as a list of (variable, coefficient) pairs with an integer constant.

type NormForm v = ([(v, Int)], Int)

-- | Type class for representing linear expressions over integer coefficients.
-- 
-- An instance of 'LinearExpr' provides constructors for constants, variable terms,
-- and basic arithmetic operations (addition, scaling). The type 'e' represents the 
-- expression type, and 'v' is the type of variables used in expressions.
-- 
-- The functional dependency @e -> v@ ensures that the variable type 'v' is uniquely
-- determined by the expression type 'e', enabling more precise type inference.
-- 
-- The class also provides a normalisation function to convert an expression into a 
-- normal form, as well as smart constructors and derived operators.

class LinearExpr e v | e -> v where
    -- | Construct a linear expression representing an integer constant.
    constant  :: Int -> e

    -- | Construct a linear term with a coefficient and a variable.
    term      :: Int -> v -> e

    -- | Add two linear expressions.
    plus      :: e -> e -> e

    -- | Scale a linear expression by an integer coefficient.
    scale     :: Int -> e -> e


    -- | Compute a normal form of a linear expression.
    normalise :: Ord v => e -> NormForm v


    -- | Smart constructor. Construct a linear expression from a normal form representation.
    linear    :: NormForm v -> e
    linear (terms, c) = foldr plus (constant c) [term coeff var | (var, coeff) <- terms]


    -- | Subtract one linear expression from another. Derived operator.
    minus     :: e -> e -> e
    minus le0 le1 = plus le0 (scale (-1) le1)


    -- | Infix operator for constructing a term.
    infixl 7 *.
    (*.) :: Int -> v -> e
    (*.) = term

    -- | Infix operator for addition of expressions.
    infixl 6 <+>
    (<+>) :: e -> e -> e
    (<+>) = plus

    -- | Infix operator for scaling an expression by an integer.
    infixl 7 <.>
    (<.>) :: Int -> e -> e
    (<.>) = scale

    -- | Infix operator for subtraction of expressions.
    infixl 6 <->
    (<->) :: e -> e -> e
    (<->) = minus


-- | The 'LinExpr' data type provides a concrete representation of linear expressions
-- over variables of type @v@.

data LinExpr v = Lit Int                       -- ^ Constant literal
               | Term Int v                    -- ^ Single term with coefficient and variable
               | Plus (LinExpr v) (LinExpr v)  -- ^ Sum of two expressions

instance LinearExpr (LinExpr v) v where
    constant :: Int -> LinExpr v
    constant = Lit

    term :: Int -> v -> LinExpr v
    term = Term

    plus :: LinExpr v -> LinExpr v -> LinExpr v
    plus = Plus

    scale :: Int -> LinExpr v -> LinExpr v
    scale c (Lit n)      = Lit $ c * n
    scale c (Term a v)   = Term (c*a) v
    scale c (Plus e0 e1) = Plus (scale c e0) (scale c e1)

    normalise :: Ord v => LinExpr v -> NormForm v
    normalise e = format $ group e (Map.empty, 0)
        where
            group (Lit c) (vm, d)    = (vm, d+c)
            group (Term a v) (vm, d) = (Map.insertWith (+) v a vm, d)
            group (Plus e0 e1) accum = group e1 (group e0 accum)
            format (vm, c)           = ([(var, coeff) | (var, coeff) <- Map.toList vm, coeff /= 0], c)
    