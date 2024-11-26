{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Criterion.Main (bench, bgroup, defaultMain, nf)
import Nonogram qualified
import Problem qualified
import SAT qualified
import Sudoku qualified

main :: IO ()
main =
  defaultMain
    [ bgroup
        "Best"
        [ bench "sudokuFour" $ nf Problem.solve Sudoku.sudokuFour,
        --   bench "sudokuNine" $ nf Problem.solve Sudoku.sudokuNine,
        --   bench "nonogram 5x5" $ nf Problem.solve Nonogram.fiveByFive,
        --   bench "nonogram 2x2" $ nf Problem.solve Nonogram.twoByTwo,
        --   bench "nonogram 5x5" $ nf Problem.solve Nonogram.fiveByFive2
          bench "sudokuSixteen" $ nf Problem.solve Sudoku.sudokuSixteen
        ],
    --   bgroup
    --     "BruteForce"
    --     [ bench "sudokuFour" $ nf Problem.solve Sudoku.sudokuFour,
    --       bench "sudokuNine" $ nf Problem.solve Sudoku.sudokuNine,
    --       bench "nonogram 5x5" $ nf Problem.solve Nonogram.fiveByFive,
    --       bench "nonogram 2x2" $ nf Problem.solve Nonogram.twoByTwo,
    --       bench "nonogram 5x5" $ nf Problem.solve Nonogram.fiveByFive2
    --       bench "sudokuSixteen" $ nf Problem.solve Sudoku.sudokuSixteen
    --     ],
      bgroup
        "Unit Propagation"
        [ bench "sudokuFour" $ nf (Problem.solveWith SAT.withUnitPropagation) Sudoku.sudokuFour,
        --   bench "sudokuNine" $ nf (Problem.solveWith SAT.withUnitPropagation) Sudoku.sudokuNine,
        --   bench "nonogram 5x5" $ nf (Problem.solveWith SAT.withUnitPropagation) Nonogram.fiveByFive,
        --   bench "nonogram 2x2" $ nf (Problem.solveWith SAT.withUnitPropagation) Nonogram.twoByTwo,
        --   bench "nonogram 5x5" $ nf (Problem.solveWith SAT.withUnitPropagation) Nonogram.fiveByFive2
            bench "sudokuSixteen" $ nf (Problem.solveWith SAT.withUnitPropagation) Sudoku.sudokuSixteen
        ],
    --   bgroup
    --     "Pure Literal Elimination"
    --     [ bench "sudokuFour" $ nf (Problem.solveWith SAT.withPureLiteralElimination) Sudoku.sudokuFour,
    --       bench "sudokuNine" $ nf (Problem.solveWith SAT.withPureLiteralElimination) Sudoku.sudokuNine,
    --       bench "nonogram 5x5" $ nf (Problem.solveWith SAT.withPureLiteralElimination) Nonogram.fiveByFive,
    --       bench "nonogram 2x2" $ nf (Problem.solveWith SAT.withPureLiteralElimination) Nonogram.twoByTwo,
    --       bench "nonogram 5x5" $ nf (Problem.solveWith SAT.withPureLiteralElimination) Nonogram.fiveByFive2
    --       bench "sudokuSixteen" $ nf Problem.solve Sudoku.sudokuSixteen
    --     ],
      bgroup
        "Pure Literal and Unit Propagation"
        [ bench "sudokuFour" $ nf (Problem.solveWith SAT.withPureLiteralAndUnitPropagation) Sudoku.sudokuFour,
        --   bench "sudokuNine" $ nf (Problem.solveWith SAT.withPureLiteralAndUnitPropagation) Sudoku.sudokuNine,
        --   bench "nonogram 5x5" $ nf (Problem.solveWith SAT.withPureLiteralAndUnitPropagation) Nonogram.fiveByFive,
        --   bench "nonogram 2x2" $ nf (Problem.solveWith SAT.withPureLiteralAndUnitPropagation) Nonogram.twoByTwo,
        --   bench "nonogram 5x5" $ nf (Problem.solveWith SAT.withPureLiteralAndUnitPropagation) Nonogram.fiveByFive2
            bench "sudokuSixteen" $ nf (Problem.solveWith SAT.withPureLiteralAndUnitPropagation) Sudoku.sudokuSixteen
        ],
      bgroup 
        "Pure literal as preprocessing"
        [ bench "sudokuFour" $ nf (Problem.solveWith SAT.pureLitOnlyAsPreprocess) Sudoku.sudokuFour,
        --   bench "sudokuNine" $ nf (Problem.solveWith SAT.pureLitOnlyAsPreprocess) Sudoku.sudokuNine,
        --   bench "nonogram 5x5" $ nf (Problem.solveWith SAT.pureLitOnlyAsPreprocess) Nonogram.fiveByFive,
        --   bench "nonogram 2x2" $ nf (Problem.solveWith SAT.pureLitOnlyAsPreprocess) Nonogram.twoByTwo,
        --   bench "nonogram 5x5" $ nf (Problem.solveWith SAT.pureLitOnlyAsPreprocess) Nonogram.fiveByFive2
            bench "sudokuSixteen" $ nf (Problem.solveWith SAT.pureLitOnlyAsPreprocess) Sudoku.sudokuSixteen
        ],
        bgroup
            "Pure literal every 100"
            [
                bench "sudokuFour" $ nf (Problem.solveWith SAT.literalEvery100) Sudoku.sudokuFour,
                bench "sudokuSixteen" $ nf (Problem.solveWith SAT.literalEvery100) Sudoku.sudokuSixteen
            ]




    ]
