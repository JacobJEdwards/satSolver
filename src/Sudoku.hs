{-# LANGUAGE OverloadedStrings #-}

module Sudoku (sudokuToCNF, sudoku, Sudoku, Variable(..), decodeSolution, encodeVar, decodeVar, prettyPrint) where

import qualified Parser.DIMACS as DIMACS

type Sudoku = [[Int]]

data Variable = Variable {
  row :: Int,
  col :: Int,
  num :: Int
} deriving (Eq, Show)


encodeVar :: Variable -> Int
encodeVar (Variable r c n) = (r - 1) * 4 * 4 + (c - 1) * 4 + (n - 1)

decodeVar :: Int -> Variable
decodeVar v = Variable (r + 1) (c + 1) (n + 1)
  where
    (r, m) = divMod v (4 * 4)
    (c, n) = divMod m 4

decodeSolution :: [(Int, Bool)] -> Sudoku
decodeSolution solution = [[cellValue r c | c <- [1..4]] | r <- [1..4]]
  where
    cellValue r c = head [n | n <- [1..4], (encodeVar $ Variable r c n, True) `elem` solution]


sudokuToCNF :: Sudoku -> DIMACS.CNF
sudokuToCNF puzzle = DIMACS.CNF {
  DIMACS.numVars = 4 * 4 * 4,
  DIMACS.numClauses = length clauses,
  DIMACS.clauses = clauses,
  DIMACS.comments = ["Sudoku"]
}
  where
    clauses = concat [
      cellClauses,       
      rowClauses,       
      colClauses,      
      blockClauses,   
      prefilledClauses
      ]

    cellClauses = [[encodeVar (Variable r c n) | n <- [1..4]] | r <- [1..4], c <- [1..4]]

    rowClauses = [[-encodeVar (Variable r c1 n), -encodeVar (Variable r c2 n)] | r <- [1..4], n <- [1..4], c1 <- [1..4], c2 <- [1..4], c1 < c2]

    colClauses = [[-encodeVar (Variable r1 c n), -encodeVar (Variable r2 c n)] | c <- [1..4], n <- [1..4], r1 <- [1..4], r2 <- [1..4], r1 < r2]

    blockClauses = [[-encodeVar (Variable r1 c1 n), -encodeVar (Variable r2 c2 n)] 
                    | n <- [1..4],
                      br <- [0, 2],
                      bc <- [0, 2],
                      r1 <- [br+1 .. br+2],
                      c1 <- [bc+1 .. bc+2],
                      r2 <- [br+1 .. br+2],
                      c2 <- [bc+1 .. bc+2],
                      (r1, c1) < (r2, c2)]

    prefilledClauses = [ [encodeVar (Variable r c n)] 
                        | r <- [1..4], c <- [1..4], 
                          let n = puzzle !! (r - 1) !! (c - 1), n /= 0 ]

prettyPrint :: Sudoku -> IO ()
prettyPrint puzzle = do
  putStrLn "Sudoku:"
  mapM_ (putStrLn . unwords . map show) puzzle
    
sudoku :: Sudoku
sudoku = [
  [1, 2, 3, 4],
  [0, 4, 0, 2],
  [0, 1, 4, 3],
  [4, 0, 2, 0]
  ]

