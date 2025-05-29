module LinearExprSpec (spec) where

import Test.Hspec
import LinearExpr
import Data.List (sort)

spec :: Spec
spec = describe "LinExpr construction and normalisation" $ do

  it "constructs a constant correctly" $ do
    normalise (constant 5 :: LinExpr String) `normFormShouldBe` ([], 5)

  it "constructs a term correctly" $ do
    normalise (3 *. "x" :: LinExpr String) `normFormShouldBe` ([("x", 3)], 0)

  it "adds two expressions with shared variables" $ do
    let e = ((-2) *. "x" <+> 3 *. "x") :: LinExpr String
    normalise e `normFormShouldBe` ([("x", 1)], 0)

  it "adds two expressions without shared variables" $ do
    let e0 = (2 *. "x" <+> constant 5) :: LinExpr String
        e1 = (3 *. "y" <+> constant 2) :: LinExpr String
    normalise (e0 <+> e1) `normFormShouldBe` ([("y", 3), ("x", 2)], 7)

  it "scales expressions by an integer" $ do
    let e = 5 <.> (2 *. "x" <+> 3 *. "y" <+> constant 1) :: LinExpr String
    normalise e `normFormShouldBe` ([("x", 10), ("y", 15)], 5)

  it "subtracts two expressions" $ do
    let e = (5 *. "x" <-> 2 *. "x") :: LinExpr String
    normalise e `normFormShouldBe` ([("x", 3)], 0)
  
  it "eliminates zero-coefficient terms" $ do
    let e = 3 *. "x" <+> 2 *. "y" <+> ((-3) *. "x") <+> constant 4 :: LinExpr String
    normalise e `normFormShouldBe` ([("y", 2)], 4)

  it "constructs from normal form" $ do
    let nf = ([("x", 2), ("y", 3)], 7)
        e = linear nf :: LinExpr String
    normalise e `normFormShouldBe` nf

-- Checks that two linear expressions in normal form represent the same expression, regardless of the order of the terms
normFormShouldBe :: (Ord v, Show v) => NormForm v -> NormForm v -> Expectation
normFormShouldBe (ts, a) (us, b) = (sort ts, a) `shouldBe` (sort us, b)
