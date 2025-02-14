{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Nonogram.Solver
-- Description : Exports the Nonogram solver module.
module Nonogram.Solver
  ( type Nonogram (..),
    type Variable (..),
    type Cell (..),
    type Constraint,
    type Size,
    type Mask,
    encodeVar,
    decodeSolution,
    Nonogram.Solver.toDIMACS,
    fiveByFive,
    fiveByFive2,
    twoByTwo,
    oneByOne,
    eightByEight,
  )
where

import Control.Parallel.Strategies (type NFData)
import Data.Kind (type Type)
import GHC.Generics (type Generic)
import SAT (checkValue, type Solutions, fromDNF)
import SAT.DIMACS qualified as DIMACS
import Utils (intNub)

imap :: (Int -> a -> b) -> [a] -> [b]
imap = flip zipWith [0 ..]

-- | Represents a cell in a nonogram.
type Cell :: Type
data Cell = Filled | Unfilled | Unknown
  deriving stock (Eq, Generic)

deriving anyclass instance NFData Cell

-- | Show instance for the 'Cell' type.
--
-- >>> show Filled
-- "#"
--
-- >>> show Unfilled
-- "."
--
-- >>> show Unknown
-- " "
instance Show Cell where
  show :: Cell -> String
  show Filled = "#"
  show Unfilled = "."
  show Unknown = " "

-- | Enum instance for the 'Cell' type.
--
-- >>> fromEnum Filled
-- 2
--
-- >>> fromEnum Unfilled
-- 1
--
-- >>> fromEnum Unknown
-- 0
instance Enum Cell where
  fromEnum :: Cell -> Int
  fromEnum Filled = 2
  fromEnum Unfilled = 1
  fromEnum Unknown = 0

  toEnum :: Int -> Cell
  toEnum 2 = Filled
  toEnum 1 = Unfilled
  toEnum 0 = Unknown
  toEnum _ = error "Invalid cell"

-- | Bounded instance for the 'Cell' type.
--
-- >>> minBound :: Cell
-- Unknown
--
-- >>> maxBound :: Cell
-- Filled
instance Bounded Cell where
  minBound :: Cell
  minBound = Unknown

  maxBound :: Cell
  maxBound = Filled

-- | Represents a constraint in a nonogram (e.g. [1, 2, 3]) (nearly CNF)
type Constraint :: Type
type Constraint = [Int]

-- | Represents the size of a nonogram.
type Size :: Type
type Size = Int

-- | Represents a mask in a nonogram.
type Mask :: Type
type Mask = [Int]

-- | Represents a nonogram.
type Nonogram :: Type
data Nonogram = Nonogram
  { rows :: ![Constraint],
    cols :: ![Constraint],
    solution :: ![[Cell]]
  }
  deriving stock (Eq, Generic)

deriving anyclass instance NFData Nonogram

-- | Show instance for the 'Nonogram' type.
--
-- >>> putStrLn $ show exampleNonogram
-- cols: [[1,1],[1,2],[3],[2,1],[1,1]]
instance Show Nonogram where
  show :: Nonogram -> String
  show (Nonogram rows' cols' solution') = "cols: " <> show cols' <> "\n" <> "rows: " <> show rows' <> "\n" <> "solution: \n" <> unlines (fmap (unwords . fmap show) solution')

-- | Represents a variable in a nonogram.
type Variable :: Type
data Variable = Variable
  { row :: !Int,
    col :: !Int,
    filled :: !Cell
  }
  deriving stock (Eq, Show, Generic)

deriving anyclass instance NFData Variable

-- | Encodes a variable to an integer.
--
-- >>> encodeVar exampleNonogram (Variable 1 1 Filled)
-- 2
encodeVar :: Nonogram -> Variable -> DIMACS.Literal
encodeVar (Nonogram rs cs _) (Variable r c f) = (r - 1) * boardWidth * 2 + (c - 1) * 2 + fromEnum f
  where
    boardWidth :: Int
    boardWidth = max (length cs) (length rs)

-- | Decodes the solution to the nonogram.
--
-- >>> decodeSolution exampleNonogram (Solutions [])
-- ...
decodeSolution :: Nonogram -> Solutions -> Nonogram
decodeSolution puzzle@(Nonogram rows' cols' _) solution' = Nonogram rows' cols' [[cellValue r c | c <- [1 .. length cols']] | r <- [1 .. length rows']]
  where
    cellValue :: Int -> Int -> Cell
    cellValue r c =
      case [f | f <- [Filled, Unfilled], checkVar $ Variable r c f] of
        [f] -> f
        xs -> error $ "Invalid values for cell (" <> show r <> ", " <> show c <> ")" <> show xs

    checkValue' :: Int -> Bool
    checkValue' = SAT.checkValue solution'

    encodeVar' :: Variable -> DIMACS.Literal
    encodeVar' = encodeVar puzzle

    checkVar :: Variable -> Bool
    checkVar = checkValue' . encodeVar'

-- | Converts a nonogram to DIMACS format.
--
-- >>> toDIMACS exampleNonogram
-- DIMACS {numVars = 20, numClauses = 0, clauses = ..., comments = ["Nonogram"]}
toDIMACS :: Nonogram -> DIMACS.DIMACS
toDIMACS !puzzle =
  DIMACS.DIMACS
    { DIMACS.numVars = fromIntegral $ rowSize * colSize * 2,
      DIMACS.numClauses = fromIntegral $ length clauses,
      DIMACS.clauses = clauses,
      DIMACS.comments = ["Nonogram"]
    }
  where
    clauses :: [DIMACS.Clause]
    clauses = concat [rowClauses, colClauses, cellClauses, cellUniqueClauses] -- ++ solutionClauses

    cellClauses :: [DIMACS.Clause]
    cellClauses = [[encodeVar' (Variable r c Filled), encodeVar' (Variable r c Unfilled)] | r <- [1 .. rowSize], c <- [1 .. colSize]]

    cellUniqueClauses :: [DIMACS.Clause]
    cellUniqueClauses = [[-encodeVar' (Variable r c Filled), -encodeVar' (Variable r c Unfilled)] | r <- [1 .. rowSize], c <- [1 .. colSize]]

    rowClauses :: [DIMACS.Clause]
    rowClauses = encodeRowConstraints encodeVar' rowSize $ rows puzzle

    colClauses :: [DIMACS.Clause]
    colClauses = encodeColConstraints encodeVar' colSize $ cols puzzle

    encodeVar' :: Variable -> DIMACS.Literal
    encodeVar' = encodeVar puzzle

    colSize :: Size
    colSize = length $ rows puzzle

    rowSize :: Size
    rowSize = length $ cols puzzle

-- has to try every possible combination of filled and empty cells
-- given a row length of 5 and a row constrain of [1,1], the possible solutions are
-- must be a gap between the two filled cells
-- [1, 0, 1, 0, 0]
-- [1, 0, 0, 1, 0]
-- [1, 0, 0, 0, 1]
-- [0, 1, 0, 1, 0]
-- [0, 1, 0, 0, 1]
-- [0, 0, 1, 0, 1]

-- | Encodes the row constraints to DIMACS format.
--
-- >>> encodeRowConstraints (encodeVar exampleNonogram) 5 [[1, 1]]
-- [[2, 4], [2, 5], [3, 5], [6, 8], [6, 9], [7, 9]]
encodeRowConstraints :: (Variable -> DIMACS.Literal) -> Size -> [Constraint] -> [DIMACS.Clause]
encodeRowConstraints encodeVar' size rows' = concat $ imap encodeRow rows'
  where
    encodeRow :: Int -> Constraint -> [DIMACS.Clause]
    encodeRow = generatePossibleSolutions size . encodeCell

    encodeCell :: Int -> Mask -> Int -> DIMACS.Literal
    encodeCell !rowIndex !mask !colIndex =
      let !cell = mask !! colIndex
      in encodeVar' $ Variable (rowIndex + 1) (colIndex + 1) $ if cell >= 1 then Filled else Unfilled

-- issue here was i was finding the index with equality, whereas i had to pass it in as i needed referential in case two
-- cells had the same value

-- | Encodes the column constraints to DIMACS format.
--
-- >>> encodeColConstraints (encodeVar exampleNonogram) 5 [[1, 1]]
-- [[2, 4], [2, 5], [3, 5], [6, 8], [6, 9], [7, 9]]
encodeColConstraints :: (Variable -> DIMACS.Literal) -> Size -> [Constraint] -> [DIMACS.Clause]
encodeColConstraints encodeVar' size cols' = concat $ imap encodeCol cols'
  where
    encodeCol :: Int -> Constraint -> [DIMACS.Clause]
    encodeCol = generatePossibleSolutions size . encodeCell

    encodeCell :: Int -> Mask -> Int -> DIMACS.Literal
    encodeCell !colIndex !mask !rowIndex =
      let !cell = mask !! rowIndex -- potentially gives incorrect results
      in encodeVar' $ Variable (rowIndex + 1) (colIndex + 1) $ if cell >= 1 then Filled else Unfilled

-- columns had the same number of filled cells
---- https://www.kbyte.io/projects/201908_nonogram/

-- | Generates all possible solutions for a given constraint.
--
-- >>> generatePossibleSolutions (encodeCell 0) 5 [1, 1]
-- [[2, 4], [2, 5], [3, 5], [6, 8], [6, 9], [7, 9]]
generatePossibleSolutions :: Size -> (Mask -> Int -> DIMACS.Literal) -> Constraint -> [DIMACS.Clause]
generatePossibleSolutions !size !encodeCell !combinations = do
  fromDNF $ map intNub $ generate combinations 1 0 $ replicate size 0 
  where 
    generate :: Constraint -> Int -> Int -> Mask -> [[DIMACS.Literal]]
    generate [] _ _ !mask = [fmap (encodeCell mask) [0 .. length mask - 1]]
    generate _ _ !limit !mask | limit >= length mask = []
    generate (c : !cs) !mark !limit !mask =
      concat
        [ generate cs (mark + 1) (startPosition + c + 1) (fillMask mask mark startPosition c)
          | startPosition <- [limit .. (length mask - 1)],
            let newMask = fillMask mask mark startPosition c,
            length newMask == length mask
        ]

    fillMask :: Mask -> Int -> Int -> Int -> Mask
    fillMask !mask !mark !startPosition !c = take startPosition mask <> replicate c mark <> drop (startPosition + c) mask

-- | Example nonogram.
fiveByFive :: Nonogram
fiveByFive =
  Nonogram
    { cols =
        [ [1, 1],
          [1, 2],
          [3],
          [2, 1],
          [1, 1]
        ],
      rows =
        [ [1, 1],
          [1, 2],
          [2],
          [5],
          [1]
        ],
      solution -- not necessary to have a solution
      =
        [ [Unknown, Unknown, Unknown, Unknown, Unknown],
          [Unknown, Unknown, Unknown, Unknown, Unknown],
          [Unknown, Unknown, Unknown, Unknown, Unknown],
          [Unknown, Unknown, Unknown, Unknown, Unknown],
          [Unknown, Unknown, Unknown, Unknown, Unknown]
        ]
    }

-- | Example nonogram.
twoByTwo :: Nonogram
twoByTwo =
  Nonogram
    { rows = [[2], [1]],
      cols = [[2], [1]],
      solution =
        [ [Unknown, Unknown],
          [Unknown, Unknown]
        ]
    }

-- 8 by 8

-- | Example nonogram.
eightByEight :: Nonogram
eightByEight =
  Nonogram
    { cols =
        [[4], [6], [4], [3], [2], [2], [3, 2], [3, 4], [6, 2], [6, 3]],
      rows =
        [[5], [5], [4], [2], [2, 2], [4, 4], [5, 2], [4, 1, 1], [2, 3], [2, 2]],
      solution =
        []
    }

-- exampleNonogram :: Nonogram
-- exampleNonogram =
--  Nonogram {
--    rows = [
--      [3], [2, 1], [3,2], [2,2], [6], [1,5], [6], [1], [2]
--      ],
--    cols = [
--      [1,2], [3,1], [1,2],[7,1], [5], [3], [4], [3]
--      ],
--    solution = [
--      [Unknown, Unknown, Unknown, Unknown, Unknown, Unknown, Unknown, Unknown, Unknown],
--      [Unknown, Unknown, Unknown, Unknown, Unknown, Unknown, Unknown, Unknown, Unknown],
--      [Unknown, Unknown, Unknown, Unknown, Unknown, Unknown, Unknown, Unknown, Unknown],
--      [Unknown, Unknown, Unknown, Unknown, Unknown, Unknown, Unknown, Unknown, Unknown],
--      [Unknown, Unknown, Unknown, Unknown, Unknown, Unknown, Unknown, Unknown, Unknown],
--      [Unknown, Unknown, Unknown, Unknown, Unknown, Unknown, Unknown, Unknown, Unknown],
--      [Unknown, Unknown, Unknown, Unknown, Unknown, Unknown, Unknown, Unknown, Unknown],
--      [Unknown, Unknown, Unknown, Unknown, Unknown, Unknown, Unknown, Unknown, Unknown]
--    ]
--  }

-- | Example nonogram.
fiveByFive2 :: Nonogram
fiveByFive2 =
  Nonogram
    { rows =
        [ [1],
          [1],
          [3],
          [4],
          [4]
        ],
      cols =
        [ [1, 2],
          [2],
          [3],
          [3],
          [1, 1]
        ],
      solution =
        [ [Unknown, Unknown, Unknown, Unknown, Unknown],
          [Unknown, Unknown, Unknown, Unknown, Unknown],
          [Unknown, Unknown, Unknown, Unknown, Unknown],
          [Unknown, Unknown, Unknown, Unknown, Unknown],
          [Unknown, Unknown, Unknown, Unknown, Unknown]
        ]
    }

-- | Example nonogram.
oneByOne :: Nonogram
oneByOne =
  Nonogram
    { rows = [[1]],
      cols = [[1]],
      solution = [[Unknown]]
    }
