{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module Problem (Problem(..), parseFile, solve, toExpr) where

import qualified SAT.DIMACS.CNF as DIMACS
import qualified SAT.DIMACS.Parser as DIMACS

import Data.Text (Text)
import qualified Data.Text as Text
import SAT.Solver (getSolutions, Solution)
import SAT.Expr (Expr)
import Sudoku.Solver (Sudoku)
import qualified Sudoku.Solver as Sudoku
import qualified Sudoku.Parser as Sudoku
import Nonogram.Solver (Nonogram)
import qualified Nonogram.Solver as Nonogram
import qualified Nonogram.Parser as Nonogram

import qualified SAT.Parser as Expr
import qualified SAT.CNF as Expr

class Problem a where
  type Variable a
  
  toCNF :: a -> DIMACS.CNF
  decode :: a -> Solution Int -> a
  encodeVar :: a -> Variable a -> Int
  example :: a
  parse :: Text -> Maybe a

instance Problem Sudoku where
  type Variable Sudoku = Sudoku.Variable
  
  toCNF :: Sudoku -> DIMACS.CNF
  toCNF = Sudoku.toCNF
  
  decode :: Sudoku -> Solution Int -> Sudoku
  decode = Sudoku.decodeSolution
  
  encodeVar :: Sudoku -> Sudoku.Variable -> Int
  encodeVar = Sudoku.encodeVar
  
  parse :: Text -> Maybe Sudoku
  parse = Sudoku.parse
  
  example :: Sudoku
  example = Sudoku.sudokuNine

instance Problem Nonogram where
  type Variable Nonogram = Nonogram.Variable
  
  toCNF :: Nonogram -> DIMACS.CNF
  toCNF = Nonogram.toCNF
  
  decode :: Nonogram -> Solution Int -> Nonogram
  decode = Nonogram.decodeSolution
  
  encodeVar :: Nonogram -> Nonogram.Variable -> Int
  encodeVar = Nonogram.encodeVar
  
  parse :: Text -> Maybe Nonogram
  parse = Nonogram.parse
  
  example :: Nonogram
  example = Nonogram.exampleNonogram

instance Problem DIMACS.CNF where
  type Variable DIMACS.CNF = Int
  
  toCNF :: DIMACS.CNF -> DIMACS.CNF
  toCNF = id
  
  decode :: DIMACS.CNF -> Solution Int -> DIMACS.CNF
  decode = const
  
  encodeVar :: DIMACS.CNF -> Int -> Int
  encodeVar = const id
  
  parse :: Text -> Maybe DIMACS.CNF
  parse = DIMACS.parse
  
  example :: DIMACS.CNF
  example = DIMACS.exampleCNF

instance Problem (Expr Int) where
  type Variable (Expr Int) = Int
  
  toCNF :: Expr Int -> DIMACS.CNF
  toCNF = DIMACS.fromExpr . Expr.toCNF
  
  decode :: Expr Int -> Solution Int -> Expr Int
  decode = const
  
  encodeVar :: Expr Int -> Int -> Int
  encodeVar = const id
  
  parse :: Text -> Maybe (Expr Int)
  parse = Expr.parse
  
  example :: Expr Int
  example = undefined

parseFile :: Problem a => Text -> IO (Maybe a)
parseFile filename = do
  contents <- readFile (Text.unpack filename)
  return $ parse (Text.pack contents)
  
solve :: (Problem a) => a -> Maybe a
solve puzzle = decode puzzle <$> getSolutions (toExpr puzzle)

toExpr :: (Problem a) => a -> Expr Int
toExpr = DIMACS.toExpr . DIMACS.clauses . toCNF