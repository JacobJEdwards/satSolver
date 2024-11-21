{-|
Module      : Problem
Description : Exports the problem module. Represents a problem that can be solved by the SAT solver.
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Problem (Problem (..), parseFile, solve, toExpr, toCNF, isSatisfiable) where

import Data.Kind (type Constraint, type Type)
import Data.Text (type Text)
import Data.Text qualified as Text
import Nonogram (type Nonogram)
import Nonogram qualified
import SAT (getSolutions, type Expr, type Solutions, type CNF)
import SAT qualified
import SAT.DIMACS.CNF qualified as DIMACS
import SAT.DIMACS.Parser qualified as DIMACS
import Sudoku (type Sudoku)
import Sudoku qualified

-- | Represents a problem that can be solved by the SAT solver.
type Problem :: Type -> Constraint
class Problem (a :: Type) where
  -- | The type of variables in the problem.
  type Variable a

  -- | Converts the problem to DIMACS format.
  toDIMACS :: a -> DIMACS.DIMACS 
  -- | Decodes the solution to the problem.
  decode :: a -> Solutions -> a
  -- | Encodes a variable to an integer.
  encodeVar :: a -> Variable a -> Int
  -- | Parses a problem from a string.
  example :: a
  -- | Parses a problem from a string.
  parse :: Text -> Maybe a

-- | Instances of the 'Problem' type class for different types of problems.

-- | Sudoku problem.
instance Problem Sudoku where
  type Variable Sudoku = Sudoku.Variable

  toDIMACS :: Sudoku -> DIMACS.DIMACS
  toDIMACS = Sudoku.toDIMACS
  {-# INLINEABLE toDIMACS #-}

  decode :: Sudoku -> Solutions -> Sudoku
  decode = Sudoku.decodeSolution
  {-# INLINEABLE decode #-}

  encodeVar :: Sudoku -> Sudoku.Variable -> Int
  encodeVar = Sudoku.encodeVar
  {-# INLINEABLE encodeVar #-}

  parse :: Text -> Maybe Sudoku
  parse = Sudoku.parse
  {-# INLINEABLE parse #-}

  example :: Sudoku
  example = Sudoku.sudokuSixteen
  {-# INLINEABLE example #-}

-- | Nonogram problem.
instance Problem Nonogram where
  type Variable Nonogram = Nonogram.Variable

  toDIMACS :: Nonogram -> DIMACS.DIMACS
  toDIMACS = Nonogram.toDIMACS
  {-# INLINEABLE toDIMACS #-}

  decode :: Nonogram -> Solutions -> Nonogram
  decode = Nonogram.decodeSolution
  {-# INLINEABLE decode #-}

  encodeVar :: Nonogram -> Nonogram.Variable -> Int
  encodeVar = Nonogram.encodeVar
  {-# INLINEABLE encodeVar #-}

  parse :: Text -> Maybe Nonogram
  parse = Nonogram.parse
  {-# INLINEABLE parse #-}

  example :: Nonogram
  example = Nonogram.exampleNonogram
  {-# INLINEABLE example #-}

-- | DIMACS problem.
instance Problem DIMACS.DIMACS where
  type Variable DIMACS.DIMACS = Int

  toDIMACS :: DIMACS.DIMACS -> DIMACS.DIMACS
  toDIMACS = id
  {-# INLINEABLE toDIMACS #-}

  decode :: DIMACS.DIMACS -> Solutions -> DIMACS.DIMACS
  decode = const
  {-# INLINEABLE decode #-}

  encodeVar :: DIMACS.DIMACS -> Int -> Int
  encodeVar = const id
  {-# INLINEABLE encodeVar #-}

  parse :: Text -> Maybe DIMACS.DIMACS
  parse = DIMACS.parse
  {-# INLINEABLE parse #-}

  example :: DIMACS.DIMACS
  example = DIMACS.exampleDIMACS
  {-# INLINEABLE example #-}

-- | SAT problem.
instance Problem (Expr Int) where
  type Variable (Expr Int) = Int

  toDIMACS :: Expr Int -> DIMACS.DIMACS
  toDIMACS = DIMACS.fromExpr . SAT.applyLaws
  {-# INLINEABLE toDIMACS #-}

  decode :: Expr Int -> Solutions -> Expr Int
  decode = const
  {-# INLINEABLE decode #-}

  encodeVar :: Expr Int -> Int -> Int
  encodeVar = const id
  {-# INLINEABLE encodeVar #-}

  parse :: Text -> Maybe (Expr Int)
  parse = SAT.parse
  {-# INLINEABLE parse #-}

  example :: Expr Int
  example = undefined
  {-# INLINEABLE example #-}

-- | CNF problem.
instance Problem CNF where
  type Variable CNF = Int

  toDIMACS :: CNF -> DIMACS.DIMACS
  toDIMACS = DIMACS.fromCNF
  {-# INLINEABLE toDIMACS #-}

  decode :: CNF -> Solutions -> CNF
  decode = const
  {-# INLINEABLE decode #-}

  encodeVar :: CNF -> Int -> Int
  encodeVar = const id
  {-# INLINEABLE encodeVar #-}

  parse :: Text -> Maybe CNF
  parse = undefined
  {-# INLINEABLE parse #-}

  example :: CNF 
  example = undefined
  {-# INLINEABLE example #-}

-- | Parses a problem from a file.
parseFile :: (Problem a) => Text -> IO (Maybe a)
parseFile filename = do
  contents <- readFile (Text.unpack filename)
  return $ parse (Text.pack contents)
{-# INLINEABLE parseFile #-}

-- | Solves a problem.
-- Returns 'Nothing' if the problem is unsatisfiable.
-- Returns 'Just' the solution if the problem is satisfiable.
-- 
-- >>> solve Sudoku.sudokuSixteen
-- Just ...
-- 
-- >>> solve Nonogram.exampleNonogram
-- Just ...
-- 
-- >>> solve DIMACS.exampleDIMACS
-- Just ...
-- 
-- >>> solve (SAT.toCNF (SAT.fromExpr (SAT.Var 1)))
-- Just 1
-- 
-- >>> solve (SAT.toCNF (SAT.fromExpr (SAT.Not (SAT.Var 1)) `SAT.And` SAT.fromExpr (SAT.Var 1)))
-- Nothing
solve :: (Problem a) => a -> Maybe a
solve puzzle = decode puzzle <$> getSolutions (toCNF puzzle)
{-# INLINEABLE solve #-}

-- | Checks if a problem is satisfiable.
-- 
-- >>> isSatisfiable Sudoku.sudokuSixteen
-- True
-- 
-- >>> isSatisfiable Nonogram.exampleNonogram
-- True
-- 
-- >>> isSatisfiable DIMACS.exampleDIMACS
-- True
-- 
-- >>> isSatisfiable (SAT.toCNF (SAT.fromExpr (SAT.Var 1)))
-- True
-- 
-- >>> isSatisfiable (SAT.toCNF (SAT.fromExpr (SAT.Not (SAT.Var 1)) `SAT.And` SAT.fromExpr (SAT.Var 1)))
-- False
isSatisfiable :: (Problem a) => a -> Bool
isSatisfiable = SAT.satisfiable . toCNF
{-# INLINEABLE isSatisfiable #-}

-- | Converts a problem to an expression.
-- 
-- >>> toExpr Sudoku.sudokuSixteen
-- ...
-- 
-- >>> toExpr Nonogram.exampleNonogram
-- ...
-- 
-- >>> toExpr DIMACS.exampleDIMACS
-- ...
-- 
-- >>> toExpr (SAT.toCNF (SAT.fromExpr (SAT.Var 1)))
-- SAT.Var 1
toExpr :: (Problem a) => a -> Expr Int
toExpr = DIMACS.toExpr . DIMACS.clauses . toDIMACS
{-# INLINEABLE toExpr #-}

-- | Converts a problem to CNF.
-- 
-- >>> toCNF Sudoku.sudokuSixteen
-- ...
-- 
-- >>> toCNF Nonogram.exampleNonogram
-- ...
-- 
-- >>> toCNF DIMACS.exampleDIMACS
-- ...
-- 
-- >>> toCNF (SAT.toCNF (SAT.fromExpr (SAT.Var 1)))
-- CNF [[1]]
-- 
-- >>> toCNF (SAT.toCNF (SAT.fromExpr (SAT.Not (SAT.Var 1)) `SAT.And` SAT.fromExpr (SAT.Var 1)))
-- CNF [[-1][1]]
toCNF :: (Problem a) => a -> CNF
toCNF = SAT.toCNF . toExpr
{-# INLINEABLE toCNF #-}