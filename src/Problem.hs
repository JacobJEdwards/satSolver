{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Problem (Problem (..), parseFile, solve, toExpr) where

import Data.Kind (type Constraint, type Type)
import Data.Text (type Text)
import Data.Text qualified as Text
import Nonogram (type Nonogram)
import Nonogram qualified
import SAT (getSolutions, type Expr, type SolutionMap)
import SAT qualified
import SAT.DIMACS.CNF qualified as DIMACS
import SAT.DIMACS.Parser qualified as DIMACS
import Sudoku (type Sudoku)
import Sudoku qualified

type Problem :: Type -> Constraint
class Problem (a :: Type) where
  type Variable a

  toCNF :: a -> DIMACS.CNF
  decode :: a -> SolutionMap Int -> a
  encodeVar :: a -> Variable a -> Int
  example :: a
  parse :: Text -> Maybe a

instance Problem Sudoku where
  type Variable Sudoku = Sudoku.Variable

  toCNF :: Sudoku -> DIMACS.CNF
  toCNF = Sudoku.toCNF
  {-# INLINEABLE toCNF #-}

  decode :: Sudoku -> SolutionMap Int -> Sudoku
  decode = Sudoku.decodeSolution
  {-# INLINEABLE decode #-}

  encodeVar :: Sudoku -> Sudoku.Variable -> Int
  encodeVar = Sudoku.encodeVar
  {-# INLINEABLE encodeVar #-}

  parse :: Text -> Maybe Sudoku
  parse = Sudoku.parse
  {-# INLINEABLE parse #-}

  example :: Sudoku
  example = Sudoku.sudokuFour
  {-# INLINEABLE example #-}

instance Problem Nonogram where
  type Variable Nonogram = Nonogram.Variable

  toCNF :: Nonogram -> DIMACS.CNF
  toCNF = Nonogram.toCNF
  {-# INLINEABLE toCNF #-}

  decode :: Nonogram -> SolutionMap Int -> Nonogram
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

instance Problem DIMACS.CNF where
  type Variable DIMACS.CNF = Int

  toCNF :: DIMACS.CNF -> DIMACS.CNF
  toCNF = id
  {-# INLINEABLE toCNF #-}

  decode :: DIMACS.CNF -> SolutionMap Int -> DIMACS.CNF
  decode = const
  {-# INLINEABLE decode #-}

  encodeVar :: DIMACS.CNF -> Int -> Int
  encodeVar = const id
  {-# INLINEABLE encodeVar #-}

  parse :: Text -> Maybe DIMACS.CNF
  parse = DIMACS.parse
  {-# INLINEABLE parse #-}

  example :: DIMACS.CNF
  example = DIMACS.exampleCNF
  {-# INLINEABLE example #-}

instance Problem (Expr Int) where
  type Variable (Expr Int) = Int

  toCNF :: Expr Int -> DIMACS.CNF
  toCNF = DIMACS.fromExpr . SAT.toCNF
  {-# INLINEABLE toCNF #-}

  decode :: Expr Int -> SolutionMap Int -> Expr Int
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

parseFile :: (Problem a) => Text -> IO (Maybe a)
parseFile filename = do
  contents <- readFile (Text.unpack filename)
  return $ parse (Text.pack contents)
{-# INLINEABLE parseFile #-}

solve :: (Problem a) => a -> Maybe a
solve puzzle = decode puzzle <$> getSolutions (toExpr puzzle)
{-# INLINEABLE solve #-}

toExpr :: (Problem a) => a -> Expr Int
toExpr = DIMACS.toExpr . DIMACS.clauses . toCNF
{-# INLINEABLE toExpr #-}