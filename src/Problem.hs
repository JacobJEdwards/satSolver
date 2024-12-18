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
import Debug.Trace (trace)

type Problem :: Type -> Constraint
class Problem (a :: Type) where
  type Variable a

  toDIMACS :: a -> DIMACS.DIMACS
  decode :: a -> Solutions Int -> a
  encodeVar :: a -> Variable a -> Int
  example :: a
  parse :: Text -> Maybe a

instance Problem Sudoku where
  type Variable Sudoku = Sudoku.Variable
  
--  toCNF :: Sudoku -> CNF Int
--  toCNF = Sudoku.toCNF
--  {-# INLINEABLE toCNF #-}

  toDIMACS :: Sudoku -> DIMACS.DIMACS
  toDIMACS = Sudoku.toDIMACS
  {-# INLINEABLE toDIMACS #-}

  decode :: Sudoku -> Solutions Int -> Sudoku
  decode = Sudoku.decodeSolution
  {-# INLINEABLE decode #-}

  encodeVar :: Sudoku -> Sudoku.Variable -> Int
  encodeVar = Sudoku.encodeVar
  {-# INLINEABLE encodeVar #-}

  parse :: Text -> Maybe Sudoku
  parse = Sudoku.parse
  {-# INLINEABLE parse #-}

  example :: Sudoku
  example = Sudoku.sudokuNine
  {-# INLINEABLE example #-}

instance Problem Nonogram where
  type Variable Nonogram = Nonogram.Variable
  
--  toCNF :: Nonogram -> CNF Int
--  toCNF = Nonogram.toCNF
--  {-# INLINEABLE toCNF #-}

  toDIMACS :: Nonogram -> DIMACS.DIMACS
  toDIMACS = Nonogram.toDIMACS
  {-# INLINEABLE toDIMACS #-}

  decode :: Nonogram -> Solutions Int -> Nonogram
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
  
--  toCNF :: DIMACS.DIMACS -> CNF Int
--  toCNF = DIMACS.toCNF
--  {-# INLINEABLE toCNF #-}

  toDIMACS :: DIMACS.DIMACS -> DIMACS.DIMACS
  toDIMACS = id
  {-# INLINEABLE toDIMACS #-}

  decode :: DIMACS.DIMACS -> Solutions Int -> DIMACS.DIMACS
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
  
--  toCNF :: Expr Int -> CNF Int
--  toCNF = SAT.toCNF
--  {-# INLINEABLE toCNF #-}

  toDIMACS :: Expr Int -> DIMACS.DIMACS
  toDIMACS = DIMACS.fromExpr . SAT.applyLaws
  {-# INLINEABLE toDIMACS #-}

  decode :: Expr Int -> Solutions Int -> Expr Int
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

instance Problem (CNF Int) where
  type Variable (CNF Int) = Int
  
--  toCNF :: CNF Int -> CNF Int
--  toCNF = id
--  {-# INLINEABLE toCNF #-}

  toDIMACS :: CNF Int -> DIMACS.DIMACS
  toDIMACS = DIMACS.fromCNF
  {-# INLINEABLE toDIMACS #-}

  decode :: CNF Int -> Solutions Int -> CNF Int
  decode = const
  {-# INLINEABLE decode #-}

  encodeVar :: CNF Int -> Int -> Int
  encodeVar = const id
  {-# INLINEABLE encodeVar #-}

  parse :: Text -> Maybe (CNF Int)
  parse = undefined
  {-# INLINEABLE parse #-}

  example :: CNF Int
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
toExpr = DIMACS.toExpr . toDIMACS
{-# INLINEABLE toExpr #-}

toCNF :: (Problem a) => a -> CNF Int
toCNF = DIMACS.toCNF . toDIMACS
{-# INLINEABLE toCNF #-}