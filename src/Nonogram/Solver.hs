{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Nonogram.Solver
  ( exampleNonogram,
    type Nonogram (..),
    type Variable (..),
    type Cell (..),
    type Constraint,
    type Size,
    type Mask,
    encodeVar,
    decodeSolution,
    Nonogram.Solver.toDIMACS,
  )
where

import Control.Lens (element, imap, (^?))
import Data.Kind (type Type)
import Data.Maybe (fromMaybe)
import SAT (ands, checkValue, ors, applyLaws, toVar, uniqueOnly, type Expr, type Solutions)
import SAT.DIMACS qualified as DIMACS

type Cell :: Type
data Cell = Filled | Unfilled | Unknown
  deriving stock (Eq)

instance Show Cell where
  show :: Cell -> String
  show Filled = "#"
  show Unfilled = "."
  show Unknown = " "

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

instance Bounded Cell where
  minBound :: Cell
  minBound = Unknown

  maxBound :: Cell
  maxBound = Filled

type Constraint :: Type
type Constraint = [Int]

type Size :: Type
type Size = Int

type Mask :: Type
type Mask = [Int]

type Nonogram :: Type
data Nonogram = Nonogram
  { rows :: [Constraint],
    cols :: [Constraint],
    solution :: [[Cell]]
  }
  deriving stock (Eq)

instance Show Nonogram where
  show :: Nonogram -> String
  show (Nonogram rows' cols' solution') = "cols: " ++ show cols' ++ "\n" ++ "rows: " ++ show rows' ++ "\n" ++ "solution: \n" ++ unlines (map (unwords . map show) solution')

type Variable :: Type
data Variable = Variable
  { row :: Int,
    col :: Int,
    filled :: Cell
  }
  deriving stock (Eq, Show)

encodeVar :: Nonogram -> Variable -> DIMACS.Literal
encodeVar (Nonogram rs cs _) (Variable r c f) = (r - 1) * boardWidth * 2 + (c - 1) * 2 + fromEnum f
  where
    boardWidth :: Int
    boardWidth = max (length cs) (length rs)

decodeSolution :: Nonogram -> Solutions DIMACS.Literal -> Nonogram
decodeSolution puzzle@(Nonogram rows' cols' _) solution' = Nonogram rows' cols' [[cellValue r c | c <- [1 .. length cols']] | r <- [1 .. length rows']]
  where
    cellValue :: Int -> Int -> Cell
    cellValue r c =
      case [f | f <- [Filled, Unfilled], checkVar $ Variable r c f] of
        [f] -> f
        xs -> error $ "Invalid values for cell (" ++ show r ++ ", " ++ show c ++ ")" ++ show xs

    checkValue' :: Int -> Bool
    checkValue' = SAT.checkValue solution'

    encodeVar' :: Variable -> DIMACS.Literal
    encodeVar' = encodeVar puzzle

    checkVar :: Variable -> Bool
    checkVar = checkValue' . encodeVar'

toDIMACS :: Nonogram -> DIMACS.DIMACS
toDIMACS puzzle =
  DIMACS.DIMACS
    { DIMACS.numVars = fromIntegral $ rowSize * colSize * 2,
      DIMACS.numClauses = fromIntegral $ length clauses,
      DIMACS.clauses = clauses,
      DIMACS.comments = ["Nonogram"]
    }
  where
    clauses = SAT.uniqueOnly $ concat [rowClauses, colClauses, cellClauses, cellUniqueClauses] -- ++ solutionClauses
    cellClauses :: [DIMACS.Clause]
    cellClauses =
      [[encodeVar' (Variable r c Filled), encodeVar' (Variable r c Unfilled)] | r <- [1 .. rowSize], c <- [1 .. colSize]]

    cellUniqueClauses :: [DIMACS.Clause]
    cellUniqueClauses = [[-encodeVar' (Variable r c Filled), -encodeVar' (Variable r c Unfilled)] | r <- [1 .. rowSize], c <- [1 .. colSize]]

    rowClauses :: [DIMACS.Clause]
    rowClauses = encodeRowConstraints encodeVar' rowSize $ rows puzzle

    colClauses :: [DIMACS.Clause]
    colClauses = encodeColConstraints encodeVar' colSize $ cols puzzle

    --    solutionClauses :: [DIMACS.Clause]
    --    solutionClauses =
    --      [ [encodeVar' (Variable r c f)]
    --        | r <- [1 .. rowSize],
    --          c <- [1 .. colSize],
    --          let f = fromMaybe Unknown (solution' ^? ix (c - 1) >>= (^? element (r - 1))),
    --          f /= Unknown
    --      ]

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

encodeRowConstraints :: (Variable -> DIMACS.Literal) -> Size -> [Constraint] -> [DIMACS.Clause]
encodeRowConstraints encodeVar' size rows' = concat $ imap encodeRow rows'
  where
    encodeRow :: Int -> Constraint -> [DIMACS.Clause]
    encodeRow rowIndex = generatePossibleSolutions (encodeCell rowIndex) size

    encodeCell :: Int -> Mask -> Int -> DIMACS.Literal
    encodeCell rowIndex mask colIndex =
      let cell = fromMaybe (-1) (mask ^? element colIndex) -- potentially gives incorrect results
          row' = rowIndex
          col' = colIndex
       in encodeVar' (Variable (row' + 1) (col' + 1) (if cell >= 1 then Filled else Unfilled))

-- issue here was i was finding the index with equality, whereas i had to pass it in as i needed referential in case two
encodeColConstraints :: (Variable -> DIMACS.Literal) -> Size -> [Constraint] -> [DIMACS.Clause]
encodeColConstraints encodeVar' size cols' = concat $ imap encodeCol cols'
  where
    encodeCol :: Int -> Constraint -> [DIMACS.Clause]
    encodeCol colIndex = generatePossibleSolutions (encodeCell colIndex) size

    encodeCell :: Int -> Mask -> Int -> DIMACS.Literal
    encodeCell colIndex mask rowIndex =
      let cell = fromMaybe (-1) (mask ^? element rowIndex) -- potentially gives incorrect results
          row' = rowIndex
          col' = colIndex
       in encodeVar' (Variable (row' + 1) (col' + 1) (if cell >= 1 then Filled else Unfilled))

-- columns had the same number of filled cells
---- https://www.kbyte.io/projects/201908_nonogram/
generatePossibleSolutions :: (Mask -> Int -> DIMACS.Literal) -> Size -> Constraint -> [DIMACS.Clause]
generatePossibleSolutions encodeCell size combinations =
  --- returns a list of AND clauses that need to be ORed together
  let generate :: Constraint -> Int -> Int -> Mask -> [[DIMACS.Literal]]
      generate [] _ _ mask = [map (encodeCell mask) [0 .. length mask - 1]]
      generate _ _ limit mask | limit >= length mask = []
      generate (c : cs) mark limit mask =
        concat
          [ generate cs (mark + 1) (startPosition + c + 1) (fillMask mask mark startPosition c)
            | startPosition <- [limit .. (length mask - 1)],
              let newMask = fillMask mask mark startPosition c,
              length newMask == length mask
          ]

      fillMask :: Mask -> Int -> Int -> Int -> Mask
      fillMask mask mark startPosition c = take startPosition mask ++ replicate c mark ++ drop (startPosition + c) mask
   in toCNF $ generate combinations 1 0 $ replicate size 0
  where
    convertToOrExpr :: [[DIMACS.Literal]] -> Expr DIMACS.Literal
    convertToOrExpr = ors . map convertToAndExpr

    convertToAndExpr :: [DIMACS.Literal] -> Expr DIMACS.Literal
    convertToAndExpr = ands . map toVar

    toCNF :: [[DIMACS.Literal]] -> [DIMACS.Clause]
    toCNF = DIMACS.clauses . DIMACS.fromExpr . applyLaws . convertToOrExpr

exampleNonogram :: Nonogram
exampleNonogram =
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

-- exampleNonogram :: Nonogram
-- exampleNonogram =
--  Nonogram
--    { rows = [[2], [1]],
--      cols = [[2], [1]],
--      solution =
--        [ [Unknown, Unknown],
--          [Unknown, Unknown]
--        ]
--    }

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

-- exampleNonogram :: Nonogram
-- exampleNonogram =
--  Nonogram {
--    rows = [
--      [1], [1], [3], [4], [4]
--      ],
--    cols = [
--      [1,2], [2], [3], [3], [1,1]
--      ],
--    solution = [
--      [Unknown, Unknown, Unknown, Unknown, Unknown],
--      [Unknown, Unknown, Unknown, Unknown, Unknown],
--      [Unknown, Unknown, Unknown, Unknown, Unknown],
--      [Unknown, Unknown, Unknown, Unknown, Unknown],
--      [Unknown, Unknown, Unknown, Unknown, Unknown]
--    ]
--  }

-- exampleNonogram :: Nonogram
-- exampleNonogram =
--  Nonogram {
--    rows = [[1]],
--    cols = [[1]],
--    solution = [[Unknown]]
--  }
