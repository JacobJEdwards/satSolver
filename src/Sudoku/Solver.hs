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
    sudokuTwentyfive,
    type Sudoku (..),
    type Size (..),
    type Variable (..),
    toDIMACS,
    decodeSolution,
    encodeVar,
    type Board,
    sudokuEmpty
  )
where

import Data.Kind (type Type)
import SAT (checkValue, type Solutions)
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
data Size = TwoByTwo | FourByFour | NineByNine | SixteenBySixteen | TwentyFiveByTwentyFive deriving stock (Eq, Show, Ord, Generic)

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
  fromEnum TwoByTwo = 2
  fromEnum FourByFour = 4
  fromEnum NineByNine = 9
  fromEnum SixteenBySixteen = 16
  fromEnum TwentyFiveByTwentyFive = 25

  toEnum :: Int -> Size
  toEnum 2 = TwoByTwo
  toEnum 4 = FourByFour
  toEnum 9 = NineByNine
  toEnum 16 = SixteenBySixteen
  toEnum 25 = TwentyFiveByTwentyFive
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
  minBound = TwoByTwo

  maxBound :: Size
  maxBound = TwentyFiveByTwentyFive

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
blockSize TwoByTwo = 1
blockSize FourByFour = 2
blockSize NineByNine = 3
blockSize SixteenBySixteen = 4
blockSize TwentyFiveByTwentyFive = 5

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
      -- uniqueOnly $
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

sudokuEmpty :: Sudoku
sudokuEmpty = 
  Sudoku
    {
      board = 
        [
          [0,0],
          [0,0]
        ],
        size = TwoByTwo
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

sudokuTwentyfive :: Sudoku
sudokuTwentyfive = 
  Sudoku
    {
      board = [
        [0,2,0,0,0, 3,14,0,8,0, 0,0,0,0,0, 0,0,13,4,24, 0,7,1,0,0],
        [0,10,17,0,0, 0,6,18,0,0, 22,16,0,12,0, 0,0,0,1,0, 0,0,13,19,0],
        [0,15,24,13,7, 0,0,0,4,0, 10,0,0,3,14, 0,18,0,0,0, 0,22,2,6,0],
        [0,0,1,21,0, 0,15,0,22,0, 0,19,13,0,0, 0,8,0,0,0, 0,16,18,20,0],
        [0,5,0,0,20, 7,25,19,0,0, 0,21,17,18,2, 10,12,22,9,15, 11,0,0,0,0],

        [11,0,0,0,22, 8,0,24,7,1, 5,0,0,0,13, 16,17,25,23,2, 4,0,6,0,19],
        [16,9,12,0,17, 0,19,22,0,0, 0,0,18,21,0, 0,20,6,13,0, 7,0,0,23,11],
        [0,0,6,0,21, 9,16,0,3,0, 0,22,20,19,0, 0,0,0,15,8, 25,0,0,0,0],
        [0,0,23,5,0, 2,0,0,11,17, 8,0,0,0,16, 12,9,0,0,21, 0,3,10,0,0],
        [0,0,0,0,0, 6,0,0,12,0, 9,1,25,0,3, 0,11,0,0,7, 0,0,21,0,0],

        [0,0,9,0,0, 23,0,5,17,4, 16,0,11,0,22, 18,2,0,21,13, 0,0,7,0,0],
        [4,6,0,0,5, 0,0,2,0,0, 0,18,21,24,0, 0,19,3,0,12, 23,0,0,17,0],
        [0,0,0,12,11, 0,7,3,0,24, 17,20,15,13,19, 1,0,5,8,0, 6,9,0,0,0],
        [0,22,0,0,14, 19,0,6,16,0, 0,8,9,7,0, 0,0,24,0,0, 3,0,0,1,18],
        [0,0,21,0,0, 25,13,0,20,8, 12,0,14,0,10, 9,16,15,0,6, 0,0,4,0,0],

        [0,0,25,0,0, 24,0,0,18,0, 4,0,3,10,5, 0,1,0,0,14, 0,0,0,0,0],
        [0,0,5,3,0, 17,0,0,23,7, 13,0,0,0,18, 19,21,0,0,22, 0,11,12,0,0],
        [0,0,0,0,18, 10,8,0,0,0, 0,25,23,2,0, 0,5,0,16,11, 9,0,3,0,0],
        [17,20,0,0,2, 0,22,16,6,0, 0,7,12,0,0, 0,0,9,3,0, 18,0,23,24,25],
        [6,0,4,0,16, 1,11,12,25,3, 19,0,0,0,21, 17,23,8,0,18, 2,0,0,0,14],

        [0,0,0,0,4, 14,24,11,19,23, 21,17,16,8,0, 0,0,1,2,9, 13,0,0,5,0],
        [0,1,14,23, 0,0,0,0,9,0, 0,0,19,5,0, 0,24,0,12,0, 0,8,17,0,0],
        [0,16,11,8,0, 0,0,0,1,0, 6,4,0,0,23, 0,15,0,0,0, 14,12,9,10,0],
        [0,21,3,0,0, 0,17,0,0,0, 0,15,0,25,20, 0,0,4,10,0, 0,0,16,11,0],
        [0,0,20,2,0, 16,5,8,0,0, 0,0,0,0,0, 0,6,0,19,25, 0,0,0,3,0]
      ],
      size = TwentyFiveByTwentyFive
    }