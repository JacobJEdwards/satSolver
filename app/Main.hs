{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import qualified Nonogram.Solver as Nonogram
import Options
import Parser.Parsec
import SAT.CNF
import qualified SAT.DIMACS.CNF as DIMACS
import qualified SAT.DIMACS.Parser as DIMACS
import SAT.Expr
import SAT.Parser
import SAT.Solver
import qualified Sudoku.Parser as Sudoku
import qualified Sudoku.Solver as Sudoku
import System.Console.Haskeline
import System.Environment (getArgs)

showResult :: (Show a, Ord a) => Result Text Text (Text, Expr a) -> IO ()
showResult result = case result of
  Result (_, expr) -> do
    putStrLn $ "Parsed: " <> show expr
    showExprInfo expr
  Errors errs -> do
    putStrLn $ "Errors: " <> show errs

showExprInfo :: (Show a, Ord a) => Expr a -> IO ()
showExprInfo expr = do
  let cnf = toCNF expr
  putStrLn $ "CNF: " <> show cnf
  let simplified = toSimple cnf
  putStrLn $ "Simplified step 1: " <> show simplified
  let simplified' = simplify simplified
  putStrLn $ "Simplified step 2: " <> show simplified'
  let satisfiable' = satisfiable expr
  putStrLn $ "Satisfiable: " <> show satisfiable'
  let solutions = getSolutions expr
  putStrLn $ "Solutions: " <> show solutions

runInteractiveMode :: IO ()
runInteractiveMode = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      outputStrLn ""
      outputStrLn "Enter a logical expression:"
      minput <- getInputLine ">>> "
      case minput of
        Nothing -> return ()
        Just "exit" -> return ()
        Just input -> do
          let result = SAT.Parser.parse $ fromString input
          liftIO $ showResult result
          loop

runDemoMode :: IO ()
runDemoMode = do
  let expr = And (Var 'A') (Or (Var 'B') (Not (Var 'C')))
  putStrLn $ "Demo expression: " <> show expr
  showExprInfo expr

runFile :: Text -> IO ()
runFile file = do
  DIMACS.parseFile file >>= \case
    Just cnf -> do
      putStrLn $ "Parsed CNF: " <> show cnf
      showExprInfo $ DIMACS.toExpr $ DIMACS.clauses cnf
    Nothing -> error "Failed to parse CNF"

runSudoku :: Text -> IO ()
runSudoku file = do
  sudokuResult <- Sudoku.parseSudokuFile file
  
  let sudoku' = fromMaybe (error "Invalid sudoku") sudokuResult
  putStrLn "Parsed sudoku:"
  print sudoku'
  
  let solution = Sudoku.solve sudoku'
  case solution of
    Just solution' -> do
      putStrLn "Solution:"
      print solution'
    Nothing -> putStrLn "No solution found"

runNonogram :: Text -> IO ()
runNonogram _ = do
  let nonogram = Nonogram.exampleNonogram
  putStrLn "Example nonogram:"
  print nonogram
  let solution = Nonogram.solve nonogram
  case solution of
    Just solution' -> do
      print solution'
    Nothing -> putStrLn "No solution found"

run :: IO ()
run = do
  args <- getArgs
  mode <- parseArgs args
  case mode of
    Interactive -> runInteractiveMode
    Demo -> runDemoMode
    RunImmediate expr -> showResult $ SAT.Parser.parse expr
    File file -> runFile file
    Options.Sudoku file -> runSudoku file
    Options.Nonogram file -> runNonogram file

main :: IO ()
main = run
