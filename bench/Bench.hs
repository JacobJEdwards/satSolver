{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Criterion.Main (bench, bgroup, defaultMain, nf, Benchmark)
import Nonogram qualified
import Problem qualified
import Problem (Problem)
import SAT qualified
import Sudoku qualified
import qualified SAT.Solver as SAT

genGroup :: [(String, SAT.Expr Int)] -> String -> (SAT.Expr Int -> Maybe SAT.Solutions) -> Benchmark
genGroup problems name solver = bgroup name $ map (\(n, p) -> bench n $ nf (Problem.solveWith solver) p) problems


displayInfo :: Problem a => a -> String -> IO ()
displayInfo p name = do 
  let cnf = Problem.toCNF p
  print $ "Problem info:" ++ name
  print $ "Number of clauses: " ++ show (Problem.getNumClauses cnf)
  print $ "Number of distinct variables: " ++ show (Problem.getNumVars cnf)
  print $ "Total number of literals: " ++ show (Problem.getTotalNumLiterals cnf)
  print $ "Average clause length: " ++ show (Problem.averageClauseLength cnf)


main :: IO ()
main = do
  displayInfo Nonogram.fiveByFive "Nonogram 5x5"
  displayInfo Nonogram.twoByTwo "Nonogram 2x2"
  displayInfo Nonogram.fiveByFive2 "Nonogram 5x5"
  displayInfo Sudoku.sudokuFour "Sudoku 4x4"
  displayInfo Sudoku.sudokuNine "Sudoku 9x9"
  displayInfo Sudoku.sudokuSixteen "Sudoku 16x16"

  let sudokuFour = Problem.toExpr Sudoku.sudokuFour
  let sudokuNine = Problem.toExpr Sudoku.sudokuNine
  let nonogramFiveByFive = Problem.toExpr Nonogram.fiveByFive
  let nonogramTwoByTwo = Problem.toExpr Nonogram.twoByTwo
  let nonogramFiveByFive2 = Problem.toExpr Nonogram.fiveByFive2
  let sudokuSixteen = Problem.toExpr Sudoku.sudokuSixteen

  let generator = genGroup [("nonogram 2x2", nonogramTwoByTwo)]

  let bruteForce = SAT.bruteForce
  let unitPropagation = SAT.withUnitPropagation
  let pureLiteralElimination = SAT.withPureLiteralElimination
  let pureLiteralAndUnitPropagation = SAT.withPureLiteralAndUnitPropagation
  let pureLitOnlyAsPreprocess = SAT.pureLitOnlyAsPreprocess
  let literalEvery100 = SAT.literalEvery100
  let tautElim = SAT.withTautologyElimination

  let solvers = [("Brute Force", bruteForce)]

  let groups = map (uncurry generator) solvers

  defaultMain groups
