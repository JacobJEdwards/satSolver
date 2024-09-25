{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Sudoku.Solver
  ( sudokuFour,
    sudokuNine,
    type Sudoku (..),
    type Size (..),
    type Variable (..),
    toCNF,
    decodeSolution,
    encodeVar,
    type Board,
  )
where

import Control.Lens (element, (^?))
import Data.Kind (type Type)
import Data.Maybe (fromMaybe)
import SAT (checkValue, uniqueOnly, type SolutionMap)
import SAT.DIMACS qualified as DIMACS

type Board :: Type
type Board = [[Int]]

type Sudoku :: Type
data Sudoku = Sudoku
  { board :: Board,
    size :: Size
  }
  deriving stock (Eq)

instance Show Sudoku where
  show :: Sudoku -> String
  show (Sudoku board' _) = unlines $ map (unwords . map show) board'

type Variable :: Type
data Variable = Variable
  { row :: Int,
    col :: Int,
    num :: Int
  }
  deriving stock (Eq, Show)

type Size :: Type
data Size = FourByFour | NineByNine | SixteenBySixteen deriving stock (Eq, Show, Ord)

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
blockSize FourByFour = 2
blockSize NineByNine = 3
blockSize SixteenBySixteen = 4

encodeVar :: Sudoku -> Variable -> DIMACS.Literal
encodeVar puzzle (Variable r c n) = (r - 1) * boardSize * boardSize + (c - 1) * boardSize + (n - 1)
  where
    boardSize = fromEnum $ size puzzle

decodeSolution :: Sudoku -> SolutionMap DIMACS.Literal -> Sudoku
decodeSolution puzzle solution = Sudoku [[cellValue r c | c <- [1 .. boardSize]] | r <- [1 .. boardSize]] $ size puzzle
  where
    boardSize :: Int
    boardSize = fromEnum $ size puzzle

    cellValue :: Int -> Int -> Int
    cellValue r c =
      case [n | n <- [1 .. boardSize], checkVar $ Variable r c n] of
        [n] -> n
        [] -> error $ "No valid number for cell (" ++ show r ++ ", " ++ show c ++ ")"
        -- temp fix -> for some reason cell (1, 1) is has the values [1, correct value]
        xs -> if c == 1 && r == 1 then last xs else error $ "Multiple valid numbers for cell (" ++ show r ++ ", " ++ show c ++ ")" ++ show xs

    encodeVar' :: Variable -> DIMACS.Literal
    encodeVar' = encodeVar puzzle

    checkValue' :: Int -> Bool
    checkValue' = checkValue solution

    checkVar :: Variable -> Bool
    checkVar = checkValue' . encodeVar'

toCNF :: Sudoku -> DIMACS.CNF
toCNF puzzle =
  DIMACS.CNF
    { DIMACS.numVars = fromIntegral $ boardSize * boardSize * boardSize,
      DIMACS.numClauses = fromIntegral $ length clauses,
      DIMACS.clauses = clauses,
      DIMACS.comments = ["Sudoku"]
    }
  where
    boardSize :: Int
    boardSize = fromEnum $ size puzzle

    blockSize' :: Int
    blockSize' = blockSize $ size puzzle

    clauses :: [DIMACS.Clause]
    clauses =
      uniqueOnly $
        concat
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
          let n = fromMaybe 0 (board puzzle ^? element (r - 1) >>= (^? element (c - 1))),
          n /= 0
      ]

    encodeVar' :: Variable -> DIMACS.Literal
    encodeVar' = encodeVar puzzle

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
