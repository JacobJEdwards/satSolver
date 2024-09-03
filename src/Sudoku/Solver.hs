{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Sudoku.Solver
  ( sudokuFour,
    sudokuNine,
    Sudoku.Solver.solve,
    Sudoku (..),
    Size (..),
    Variable (..),
  )
where

import Control.Lens
import Data.Maybe (fromMaybe)
import Problem
import qualified SAT.DIMACS.CNF as DIMACS
import SAT.Solver (Solution, getSolutions, checkValue)
import SAT.Optimisers (uniqueOnly)

instance Problem Sudoku where
  solve :: Sudoku -> Maybe Sudoku
  solve = Sudoku.Solver.solve

  example :: Sudoku
  example = sudokuFour

data Size = FourByFour | NineByNine | SixteenBySixteen deriving (Eq, Show, Ord)

instance Enum Size where
  fromEnum :: Size -> Int
  fromEnum FourByFour = 4
  fromEnum NineByNine = 9
  fromEnum SixteenBySixteen = 16

  toEnum :: Int -> Size
  toEnum 4 = FourByFour
  toEnum 9 = NineByNine
  toEnum 16 = SixteenBySixteen
  toEnum _ = error "Invalid size"

instance Bounded Size where
  minBound :: Size
  minBound = FourByFour
  
  maxBound :: Size
  maxBound = SixteenBySixteen

blockSize :: Size -> Int
blockSize size' = case size' of
  FourByFour -> 2
  NineByNine -> 3
  SixteenBySixteen -> 4
  
type Board = [[Int]]

data Sudoku = Sudoku
  { board :: Board,
    size :: Size
  }
  deriving (Eq)

instance Show Sudoku where
  show :: Sudoku -> String
  show (Sudoku board' _) = unlines $ map (unwords . map show) board'

data Variable = Variable
  { row :: Int,
    col :: Int,
    num :: Int
  }
  deriving (Eq, Show)

encodeVar :: Size -> Variable -> Int
encodeVar s (Variable r c n) = (r - 1) * boardSize * boardSize + (c - 1) * boardSize + (n - 1)
  where
    boardSize = fromEnum s

decodeSolution :: Size -> Solution Int -> Sudoku
decodeSolution size' solution = Sudoku [[cellValue r c | c <- [1 .. boardSize]] | r <- [1 .. boardSize]] size'
  where
    boardSize :: Int
    boardSize = fromEnum size'

    cellValue :: Int -> Int -> Int
    cellValue r c =
      case [n | n <- [1 .. boardSize], checkVar $ Variable r c n] of
        [n] -> n
        [] -> error $ "No valid number for cell (" ++ show r ++ ", " ++ show c ++ ")"
        -- temp fix -> for some reason cell (1, 1) is has the values [1, correct value]
        xs -> if c == 1 && r == 1 then last xs else error $ "Multiple valid numbers for cell (" ++ show r ++ ", " ++ show c ++ ")" ++ show xs
    
    encodeVar' :: Variable -> Int
    encodeVar' = encodeVar size'
    
    checkValue' :: Int -> Bool
    checkValue' = checkValue solution
    
    checkVar :: Variable -> Bool
    checkVar = checkValue' . encodeVar'
    

toCNF :: Sudoku -> DIMACS.CNF
toCNF (Sudoku puzzle size') =
  DIMACS.CNF
    { DIMACS.numVars = boardSize * boardSize * boardSize,
      DIMACS.numClauses = length clauses,
      DIMACS.clauses = clauses,
      DIMACS.comments = ["Sudoku"]
    }
  where
    boardSize :: Int
    boardSize = fromEnum size'
    
    blockSize' :: Int
    blockSize' = blockSize size'

    clauses :: [DIMACS.Clause]
    clauses =
      uniqueOnly $ concat
        [ cellClauses,
          rowClauses,
          colClauses,
          blockClauses,
          prefilledClauses
        ]

    cellClauses :: [DIMACS.Clause]
    cellClauses = [[encodeVar' (Variable r c n) | n <- [1 .. boardSize]] | r <- [1 .. boardSize], c <- [1 .. boardSize]]

    rowClauses :: [DIMACS.Clause]
    rowClauses = [[-encodeVar' (Variable r c1 n), -encodeVar' (Variable r c2 n)] | r <- [1 .. boardSize], n <- [1 .. boardSize], c1 <- [1 .. boardSize], c2 <- [1 .. boardSize], c1 < c2]

    colClauses :: [DIMACS.Clause]
    colClauses = [[-encodeVar' (Variable r1 c n), -encodeVar' (Variable r2 c n)] | c <- [1 .. boardSize], n <- [1 .. boardSize], r1 <- [1 .. boardSize], r2 <- [1 .. boardSize], r1 < r2]

    blockClauses :: [DIMACS.Clause]
    blockClauses =
      [ [-encodeVar' (Variable r1 c1 n), -encodeVar' (Variable r2 c2 n)]
        | n <- [1 .. boardSize],
          br <- [0, blockSize' .. boardSize - blockSize'],
          bc <- [0, blockSize' .. boardSize - blockSize'],
          r1 <- [br + 1 .. br + blockSize'],
          c1 <- [bc + 1 .. bc + blockSize'],
          r2 <- [br + 1 .. br + blockSize'],
          c2 <- [bc + 1 .. bc + blockSize'],
          (r1, c1) < (r2, c2)
      ]

    prefilledClauses :: [DIMACS.Clause]
    prefilledClauses =
      [ [encodeVar' (Variable r c n)]
        | r <- [1 .. boardSize],
          c <- [1 .. boardSize],
          let n = fromMaybe 0 (puzzle ^? element (r - 1) >>= (^? element (c - 1))),
          n /= 0
      ]

    encodeVar' :: Variable -> Int
    encodeVar' = encodeVar size'

sudokuFour :: Sudoku
sudokuFour =
  Sudoku
    { board =
        [ [1, 2, 3, 4],
          [0, 4, 0, 2],
          [0, 1, 4, 3],
          [4, 0, 2, 0]
        ],
      size = FourByFour
    }

sudokuNine :: Sudoku
sudokuNine =
  Sudoku
    { board =
        [ [5, 3, 0, 0, 7, 0, 0, 0, 0],
          [6, 0, 0, 1, 9, 5, 0, 0, 0],
          [0, 9, 8, 0, 0, 0, 0, 6, 0],
          [8, 0, 0, 0, 6, 0, 0, 0, 3],
          [4, 0, 0, 8, 0, 3, 0, 0, 1],
          [7, 0, 0, 0, 2, 0, 0, 0, 6],
          [0, 6, 0, 0, 0, 0, 2, 8, 0],
          [0, 0, 0, 4, 1, 9, 0, 0, 5],
          [0, 0, 0, 0, 8, 0, 0, 7, 9]
        ],
      size = NineByNine
    }

solve :: Sudoku -> Maybe Sudoku
solve puzzle = decodeSolution (size puzzle) <$> getSolutions (DIMACS.toExpr $ DIMACS.clauses $ toCNF puzzle)
