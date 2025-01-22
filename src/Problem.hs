{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Problem
-- Description : Exports the problem module. Represents a problem that can be solved by the SAT solver.
module Problem (type Problem (toDIMACS, decode, example, parse), parseFile, solve, toExpr, toCNF, isSatisfiable, getNumClauses, getNumVars, solveWith, averageClauseLength, getTotalNumLiterals) where

import Data.Kind (type Constraint, type Type)
import Data.List (genericLength)
import Data.Text (type Text)
import Data.Text qualified as Text
import Nonogram (type Nonogram)
import Nonogram qualified
import SAT (getSolutions, type CNF, type Expr, type Solutions)
import SAT qualified
import SAT.DIMACS.CNF qualified as DIMACS
import SAT.DIMACS.Parser qualified as DIMACS
import SAT.Encode (type Encodable (encode, type Code))
import Sudoku qualified
import Sudoku.Solver (type Sudoku)
import Debug.Trace (trace)

-- | Represents a problem that can be solved by the SAT solver.
type Problem :: Type -> Constraint
class Problem (a :: Type) where
  -- | Converts the problem to DIMACS format.
  toDIMACS :: a -> DIMACS.DIMACS

  -- | Decodes the solution to the problem.
  decode :: a -> Solutions -> a

  -- | Parses a problem from a string.
  example :: a

  -- | Parses a problem from a string.
  parse :: Text -> Maybe a

-- | Instances of the 'Problem' type class for different types of problems.

-- | Sudoku problem.
instance Problem Sudoku where
  toDIMACS :: Sudoku -> DIMACS.DIMACS
  toDIMACS = Sudoku.toDIMACS
  {-# INLINEABLE toDIMACS #-}

  decode :: Sudoku -> Solutions -> Sudoku
  decode = Sudoku.decodeSolution
  {-# INLINEABLE decode #-}

  parse :: Text -> Maybe Sudoku
  parse = Sudoku.parse
  {-# INLINEABLE parse #-}

  example :: Sudoku
  -- example = Sudoku.sudokuSixteen
  example = Sudoku.sudokuTwentyfive
  {-# INLINEABLE example #-}

-- | Nonogram problem.
instance Problem Nonogram where
  toDIMACS :: Nonogram -> DIMACS.DIMACS
  toDIMACS = Nonogram.toDIMACS
  {-# INLINEABLE toDIMACS #-}

  decode :: Nonogram -> Solutions -> Nonogram
  decode = Nonogram.decodeSolution
  {-# INLINEABLE decode #-}

  parse :: Text -> Maybe Nonogram
  parse = Nonogram.parse
  {-# INLINEABLE parse #-}

  example :: Nonogram
  example = Nonogram.eightByEight
  {-# INLINEABLE example #-}

-- | DIMACS problem.
instance Problem DIMACS.DIMACS where
  toDIMACS :: DIMACS.DIMACS -> DIMACS.DIMACS
  toDIMACS = id
  {-# INLINEABLE toDIMACS #-}

  decode :: DIMACS.DIMACS -> Solutions -> DIMACS.DIMACS
  decode = const
  {-# INLINEABLE decode #-}

  parse :: Text -> Maybe DIMACS.DIMACS
  parse = DIMACS.parse
  {-# INLINEABLE parse #-}

  example :: DIMACS.DIMACS
  example = DIMACS.exampleDIMACS
  {-# INLINEABLE example #-}

-- | SAT problem.
instance (Read a, Encodable a, Num a, Integral (Code a)) => Problem (Expr a) where
  toDIMACS :: Expr a -> DIMACS.DIMACS
  toDIMACS = DIMACS.fromExpr . SAT.applyLaws . fmap (fromIntegral . encode)
  {-# INLINEABLE toDIMACS #-}

  decode :: Expr a -> Solutions -> Expr a
  decode = const
  {-# INLINEABLE decode #-}

  parse :: Text -> Maybe (Expr a)
  parse = SAT.parse
  {-# INLINEABLE parse #-}

  example :: Expr a
  example = SAT.And (SAT.Var 1) (SAT.Var 2)
  {-# INLINEABLE example #-}

-- | CNF problem.
instance Problem CNF where
  toDIMACS :: CNF -> DIMACS.DIMACS
  toDIMACS = DIMACS.fromCNF
  {-# INLINEABLE toDIMACS #-}

  decode :: CNF -> Solutions -> CNF
  decode = const
  {-# INLINEABLE decode #-}

  parse :: Text -> Maybe CNF
  parse = undefined
  {-# INLINEABLE parse #-}

  example :: CNF
  example = undefined
  {-# INLINEABLE example #-}

-- | Parses a problem from a file.
parseFile :: (Problem a) => Text -> IO (Maybe a)
parseFile filename = do
  contents <- readFile $ Text.unpack filename
  return $ parse $ Text.pack contents
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

solveWith :: (Problem a) => (Expr Int -> Maybe Solutions) -> a -> Maybe a
solveWith f puzzle = decode puzzle <$> f (toExpr puzzle)
{-# INLINEABLE solveWith #-}

-- | Gets the number of clauses in a problem.
--
-- >>> getNumClauses Sudoku.sudokuSixteen
-- ...
getNumClauses :: (Problem a) => a -> Integer
getNumClauses = genericLength . DIMACS.clauses . toDIMACS
{-# INLINEABLE getNumClauses #-}

getTotalNumLiterals :: (Problem a) => a -> Integer
getTotalNumLiterals = sum . map genericLength . DIMACS.clauses . toDIMACS
{-# INLINEABLE getTotalNumLiterals #-}

getNumVars :: (Problem a) => a -> Integer
getNumVars = DIMACS.numVars . toDIMACS
{-# INLINEABLE getNumVars #-}

averageClauseLength :: (Problem a) => a -> Double
averageClauseLength p = fromIntegral (getTotalNumLiterals p) / fromIntegral (getNumClauses p)
{-# INLINEABLE averageClauseLength #-}

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
toCNF = DIMACS.toCNF . toDIMACS
{-# INLINEABLE toCNF #-}

simplify :: (Problem a) => a -> a
simplify = undefined
{-# INLINEABLE simplify #-}
