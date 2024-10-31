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

type Problem :: Type -> Constraint
class Problem (a :: Type) where
  type Variable a

  toDIMACS :: a -> DIMACS.DIMACS
  decode :: a -> Solutions -> a
  encodeVar :: a -> Variable a -> Int
  example :: a
  parse :: Text -> Maybe a

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

parseFile :: (Problem a) => Text -> IO (Maybe a)
parseFile filename = do
  contents <- readFile (Text.unpack filename)
  return $ parse (Text.pack contents)
{-# INLINEABLE parseFile #-}

solve :: (Problem a) => a -> Maybe a
solve puzzle = decode puzzle <$> getSolutions (toCNF puzzle)
{-# INLINEABLE solve #-}

isSatisfiable :: (Problem a) => a -> Bool
isSatisfiable = SAT.satisfiable . toCNF
{-# INLINEABLE isSatisfiable #-}

toExpr :: (Problem a) => a -> Expr Int
toExpr = DIMACS.toExpr . DIMACS.clauses . toDIMACS
{-# INLINEABLE toExpr #-}

toCNF :: (Problem a) => a -> CNF
toCNF = SAT.toCNF . toExpr
{-# INLINEABLE toCNF #-}