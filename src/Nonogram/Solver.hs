{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Nonogram.Solver (exampleNonogram, Nonogram(..), Variable(..), Cell(..), Constraint, Size, Mask, encodeVar, decodeSolution, toCNF) where

import Control.Lens
import Data.Maybe (fromMaybe)
import qualified SAT.CNF as CNF
import qualified SAT.DIMACS.CNF as DIMACS
import qualified SAT.Expr as Expr
import SAT.Solver (Solution, checkValue)
import SAT.Optimisers (uniqueOnly)
import Data.List (transpose)

data Cell = Filled | Unfilled | Unknown
  deriving (Eq)

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

type Constraint = [Int]

type Size = Int

type Mask = [Int]

data Nonogram = Nonogram
  { rows :: [Constraint],
    cols :: [Constraint],
    solution :: [[Cell]]
  }
  deriving (Eq)

instance Show Nonogram where
  show :: Nonogram -> String
  show (Nonogram rows' cols' solution') = "cols: " ++ show cols' ++ "\n" ++ "rows: " ++ show rows' ++ "\n" ++ "solution: \n" ++ unlines (map (unwords . map show) solution')

-- instance Show Nonogram where
--  show :: Nonogram -> String
--  show (Nonogram rows' cols' solution') =
--    unlines $ formatColumns cols' ++ formatGrid rows' solution'

formatColumns :: [Constraint] -> [String]
formatColumns cols' =
  let maxLength = maximum (map length cols')
      formattedCols = map (padLeft maxLength . map show) cols'
      transposedCols = transpose formattedCols
   in map unwords transposedCols

formatGrid :: [Constraint] -> [[Cell]] -> [String]
formatGrid = zipWith formatRow

formatRow :: Constraint -> [Cell] -> String
formatRow row' solution' =
  let rowStr = unwords $ map show row'
      solutionStr = unwords $ map show solution'
   in rowStr ++ " " ++ solutionStr

padLeft :: Int -> [String] -> [String]
padLeft n strs = replicate (n - length strs) " " ++ strs

data Variable = Variable
  { row :: Int,
    col :: Int,
    filled :: Cell
  }
  deriving (Eq, Show)

encodeVar :: Nonogram -> Variable -> Int
encodeVar (Nonogram rs cs _) (Variable r c f) = (r - 1) * boardWidth * 2 + (c - 1) * 2 + (if f == Filled then 2 else 1)
  where
    boardWidth :: Int
    boardWidth = max (length cs) (length rs)

decodeSolution :: Nonogram -> Solution Int -> Nonogram
decodeSolution puzzle@(Nonogram rows' cols' _) solution' = Nonogram rows' cols' [[cellValue r c | c <- [1 .. length cols']] | r <- [1 .. length rows']]
  where
    cellValue :: Int -> Int -> Cell
    cellValue r c =
      case [f | f <- [Filled, Unfilled], checkVar $ Variable r c f] of
        [f] -> f
        xs -> error $ "Invalid values for cell (" ++ show r ++ ", " ++ show c ++ ")" ++ show xs

    checkValue' :: Int -> Bool
    checkValue' = checkValue solution'

    encodeVar' :: Variable -> Int
    encodeVar' = encodeVar puzzle
    
    checkVar :: Variable -> Bool
    checkVar = checkValue' . encodeVar'

toCNF :: Nonogram -> DIMACS.CNF
toCNF puzzle =
  DIMACS.CNF
    { DIMACS.numVars = rowSize * colSize * 2,
      DIMACS.numClauses = length clauses,
      DIMACS.clauses = clauses,
      DIMACS.comments = ["Nonogram"]
    }
  where
    clauses = uniqueOnly $ concat [rowClauses, colClauses, cellClauses, cellUniqueClauses] -- ++ solutionClauses
    
    cellClauses :: [DIMACS.Clause]
    cellClauses = 
      [[encodeVar' (Variable r c Filled), encodeVar' (Variable r c Unfilled)] | r <- [1 .. rowSize], c <- [1 .. colSize]]
    
    cellUniqueClauses :: [DIMACS.Clause]
    cellUniqueClauses =  [[-encodeVar' (Variable r c Filled), -encodeVar' (Variable r c Unfilled)] | r <- [1 .. rowSize], c <- [1 .. colSize]]

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

    encodeVar' :: Variable -> Int
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


encodeRowConstraints :: (Variable -> Int) -> Size -> [Constraint] -> [DIMACS.Clause]
encodeRowConstraints encodeVar' size rows' = concat $ imap encodeRow rows'
  where
    encodeRow :: Int -> Constraint -> [DIMACS.Clause]
    encodeRow rowIndex = generatePossibleSolutions (encodeCell rowIndex) size

    encodeCell :: Int -> Mask -> Int -> Int
    encodeCell rowIndex mask colIndex =
      let cell = fromMaybe (-1) (mask ^? element colIndex) -- potentially gives incorrect results
          row' = rowIndex
          col' = colIndex
       in encodeVar' (Variable (row' + 1) (col' + 1) (if cell >= 1 then Filled else Unfilled))

-- issue here was i was finding the index with equality, whereas i had to pass it in as i needed referential in case two
encodeColConstraints :: (Variable -> Int) -> Size -> [Constraint] -> [DIMACS.Clause]
encodeColConstraints encodeVar' size cols' = concat $ imap encodeCol cols'
  where
    encodeCol :: Int -> Constraint -> [DIMACS.Clause]
    encodeCol colIndex = generatePossibleSolutions (encodeCell colIndex) size

    encodeCell :: Int -> Mask -> Int -> Int
    encodeCell colIndex mask rowIndex =
      let cell = fromMaybe (-1) (mask ^? element rowIndex) -- potentially gives incorrect results
          row' = rowIndex
          col' = colIndex
       in encodeVar' (Variable (row' + 1) (col' + 1) (if cell >= 1 then Filled else Unfilled))

-- columns had the same number of filled cells
---- https://www.kbyte.io/projects/201908_nonogram/
generatePossibleSolutions :: (Mask -> Int -> Int) -> Size -> Constraint -> [DIMACS.Clause]
generatePossibleSolutions encodeCell size combinations =
  --- returns a list of AND clauses that need to be ORed together
  let generate :: Constraint -> Int -> Int -> Mask -> [[Int]]
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
   in DIMACS.clauses $ DIMACS.fromExpr $ convertToCnf $ generate combinations 1 0 (replicate size 0)
  where
    convertToOrExpr :: [[Int]] -> Expr.Expr Int
    convertToOrExpr = foldr1 Expr.Or . map convertToAndExpr

    convertToAndExpr :: [Int] -> Expr.Expr Int
    convertToAndExpr = foldr1 Expr.And . map toExpr'

    toExpr' :: Int -> Expr.Expr Int
    toExpr' n
      | n < 0 = Expr.Not (Expr.Var (abs n))
      | otherwise = Expr.Var n

    convertToCnf :: [[Int]] -> Expr.Expr Int
    convertToCnf = CNF.toCNF . convertToOrExpr

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
