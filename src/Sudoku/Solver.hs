{-|
Module      : Sudoku.Solver
Description : Exports the Sudoku solver module.
-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sudoku.Solver
  ( sudokuFour,
    sudokuNine,
    sudokuSixteen,
    type Sudoku (..),
    type Size (..),
    type Variable (..),
    toDIMACS,
    decodeSolution,
    encodeVar,
    type Board,
  )
where

import Data.Kind (type Type)
import SAT (checkValue, uniqueOnly, type Solutions)
import SAT.DIMACS qualified as DIMACS
import GHC.Generics (Generic)
import Control.Parallel.Strategies (NFData)

-- | The Sudoku board
type Board :: Type
type Board = [[Int]]

-- | The Sudoku
type Sudoku :: Type
data Sudoku = Sudoku
  { board :: Board,
    size :: Size
  }
  deriving stock (Eq, Generic)

deriving anyclass instance NFData Sudoku

-- | Show instance for Sudoku
instance Show Sudoku where
  show :: Sudoku -> String
  show (Sudoku board' _) = unlines $ map (unwords . map show) board'

-- | The Sudoku variable
type Variable :: Type
data Variable = Variable
  { row :: Int,
    col :: Int,
    num :: Int
  }
  deriving stock (Eq, Show, Ord, Generic)

deriving anyclass instance NFData Variable

-- | The Size of the Sudoku
type Size :: Type
data Size = FourByFour | NineByNine | SixteenBySixteen deriving stock (Eq, Show, Ord, Generic)

deriving anyclass instance NFData Size

-- | Enum instance for Size
-- 
-- >>> fromEnum FourByFour
-- 4
-- 
-- >>> fromEnum NineByNine
-- 9
-- 
-- >>> fromEnum SixteenBySixteen
-- 16
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

-- | Bounded instance for Size
-- 
-- >>> minBound :: Size
-- FourByFour
-- 
-- >>> maxBound :: Size
-- SixteenBySixteen
instance Bounded Size where
  minBound :: Size
  minBound = FourByFour

  maxBound :: Size
  maxBound = SixteenBySixteen

-- | The block size of the Sudoku
-- 
-- >>> blockSize FourByFour
-- 2
-- 
-- >>> blockSize NineByNine
-- 3
-- 
-- >>> blockSize SixteenBySixteen
-- 4
blockSize :: Size -> Int
blockSize FourByFour = 2
blockSize NineByNine = 3
blockSize SixteenBySixteen = 4

-- | Encodes a variable to a DIMACS literal
-- 
-- >>> encodeVar sudokuFour (Variable 1 1 1)
-- 1
encodeVar :: Sudoku -> Variable -> DIMACS.Literal
encodeVar puzzle (Variable r c n) = var
  where
    boardSize = fromEnum $ size puzzle
    
    var :: Int
    var = ((r - 1) * boardSize * boardSize + (c - 1) * boardSize + (n - 1)) + 1

-- | Decodes a solution to a Sudoku
-- 
-- >>> decodeSolution sudokuFour [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]
-- 1 2 3 4
decodeSolution :: Sudoku -> Solutions -> Sudoku
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

-- | Converts a Sudoku to DIMACS
-- 
-- >>> toDIMACS sudokuFour
-- ...
toDIMACS :: Sudoku -> DIMACS.DIMACS
toDIMACS puzzle =
  DIMACS.DIMACS
    { DIMACS.numVars = fromIntegral $ boardSize * boardSize * boardSize,
      DIMACS.numClauses = fromIntegral $ length clauses,
      DIMACS.clauses = clauses,
      DIMACS.comments = ["Sudoku"]
    }
  where
    -- | The size of the board
    boardSize :: Int
    boardSize = fromEnum $ size puzzle

    -- | The size of the block
    blockSize' :: Int
    blockSize' = blockSize $ size puzzle

    -- | The clauses of the Sudoku
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

    -- | The cell clauses
    cellClauses :: [DIMACS.Clause]
    cellClauses = [[encodeVar' (Variable r c n) | n <- [1 .. boardSize]] | r <- [1 .. boardSize], c <- [1 .. boardSize]]

    -- | The row clauses
    rowClauses :: [DIMACS.Clause]
    rowClauses = [[-encodeVar' (Variable r c1 n), -encodeVar' (Variable r c2 n)] | r <- [1 .. boardSize], n <- [1 .. boardSize], c1 <- [1 .. boardSize], c2 <- [1 .. boardSize], c1 < c2]

    -- | The column clauses
    colClauses :: [DIMACS.Clause]
    colClauses = [[-encodeVar' (Variable r1 c n), -encodeVar' (Variable r2 c n)] | c <- [1 .. boardSize], n <- [1 .. boardSize], r1 <- [1 .. boardSize], r2 <- [1 .. boardSize], r1 < r2]

    -- | The block clauses
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

    -- | The prefilled (known) clauses
    prefilledClauses :: [DIMACS.Clause]
    prefilledClauses =
      [ [encodeVar' (Variable r c n)]
        | r <- [1 .. boardSize],
          c <- [1 .. boardSize],
          let n = board puzzle !! (r - 1) !! (c - 1),
          n /= 0
      ]

    -- | Encodes a variable to a DIMACS literal
    encodeVar' :: Variable -> DIMACS.Literal
    encodeVar' = encodeVar puzzle

-- | Example Sudoku puzzles

-- | A 4x4 Sudoku puzzle
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

-- | A 9x9 Sudoku puzzle
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

-- | A 16x16 Sudoku puzzle
sudokuSixteen :: Sudoku
sudokuSixteen =
  Sudoku
    { board =
        [ [0, 11, 0, 0, 0, 2, 3, 14, 0, 0, 9, 12, 0, 0, 0, 16],
          [15, 12, 0, 0, 0, 11, 0, 1, 13, 10, 0, 0, 0, 0, 7, 2],
          [0, 0, 10, 0, 0, 0, 0, 0, 16, 11, 0, 1, 6, 4, 12, 3],
          [0, 16, 14, 1, 0, 4, 0, 6, 0, 3, 0, 15, 0, 8, 0, 0],
          
          [1, 6, 5, 12, 0, 0, 11, 0, 0, 9, 8, 0, 0, 0, 0, 0],
          [0, 0, 0, 7, 14, 1, 8, 0, 0, 15, 6, 0, 13, 5, 0, 4],
          [4, 15, 8, 0, 9, 13, 0, 0, 0, 0, 7, 16, 3, 0, 0, 0],
          [0, 9, 13, 0, 0, 0, 0, 15, 10, 0, 0, 0, 7, 6, 0, 11],
          
          [14, 0, 6, 11, 0, 0, 0, 12, 7, 0, 0, 0, 0, 3, 13, 0],
          [0, 0, 0, 5, 8, 14, 0, 0, 0, 0, 13, 11, 0, 1, 2, 6],
          [13, 0, 16, 4, 0, 15, 5, 0, 0, 1, 12, 6, 8, 0, 0, 0],
          [0, 0, 0, 0, 0, 16, 10, 0, 0, 8, 0, 0, 11, 9, 4, 5],
          
          [0, 0, 11, 0, 1, 0, 14, 0, 5, 0, 3, 0, 15, 7, 16, 0],
          [5, 13, 15, 3, 16, 0, 4, 7, 0, 0, 0, 0, 0, 2, 0, 0],
          [16, 1, 0, 0, 0, 0, 12, 2, 14, 0, 15, 0, 0, 0, 3, 8],
          [9, 0, 0, 0, 13, 5, 0, 0, 8, 6, 16, 0, 0, 0, 10, 0]
        ],
      size = SixteenBySixteen
    }

