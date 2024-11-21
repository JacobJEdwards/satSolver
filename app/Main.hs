{-|
Module      : Main
Description : Main module for the SAT solver application.

This module contains the main function for the SAT solver application.
-}

{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (type Text)
import Nonogram (type Nonogram)
import Options (parseArgs, type Flag (Demo, File, Interactive, Nonogram, RunImmediate, Sudoku))
import Problem (example, parse, parseFile, solve)
import SAT (type Expr (And, Not, Or, Var))
import SAT qualified
import SAT.CNF qualified
import SAT.DIMACS qualified as DIMACS
import Sudoku (type Sudoku)
import System.Console.Haskeline (defaultSettings, getInputLine, outputStrLn, runInputT, type InputT)
import System.Environment (getArgs)

-- | Show the result of parsing an expression.
-- If the expression is invalid, print an error message.
showResult :: Maybe (Expr Int) -> IO ()
showResult = maybe (putStrLn "Failed to parse expression") showExprInfo

-- | Show information about an expression.
-- This includes the CNF representation, the free variable, whether the expression is satisfiable,
-- and the solutions to the expression.
showExprInfo :: Expr Int -> IO ()
showExprInfo expr = do
  let cnf = SAT.CNF.toCNF expr
  putStrLn $ "CNF: " <> show cnf
  let freeVar = SAT.findFreeVariable cnf
  putStrLn $ "Free variable: " <> show freeVar
  let satisfiable' = SAT.satisfiable cnf
  putStrLn $ "Satisfiable: " <> show satisfiable'
  let solutions = SAT.getSolutions cnf
  putStrLn $ "Solutions: " <> show solutions

-- | Run the interactive mode of the application.
-- The user can enter logical expressions to parse and evaluate.
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
          let result = parse $ fromString input :: Maybe (Expr Int)
          liftIO $ showResult result
          loop

-- | Run the demo mode of the application.
-- A demo expression is parsed and evaluated.
runDemoMode :: IO ()
runDemoMode = do
  let expr = And (Or (Var 1) (Var 2)) (Not (Var 3))

  putStrLn $ "Demo expression: " <> show expr
  showExprInfo expr

-- | Run the application with a given file.
-- The file is parsed and the CNF representation is shown.
runFile :: Text -> IO ()
runFile file = do
  DIMACS.parseFile file >>= \case
    Just cnf -> do
      putStrLn $ "Parsed CNF: " <> show cnf
      showExprInfo $ DIMACS.toExpr $ DIMACS.clauses cnf
    Nothing -> error "Failed to parse CNF"

-- | Run the sudoku problem.
-- If a file is provided, parse the sudoku from the file.
-- Otherwise, use the default sudoku.
-- Solve the sudoku and print the solution.
runSudoku :: Maybe Text -> IO ()
runSudoku file = do
  sudokuResult <- case file of
    Just file' -> parseFile file' :: IO (Maybe Sudoku)
    Nothing -> do
      putStrLn "No file provided, using default sudoku"
      return $ pure example

  let sudoku' = fromMaybe (error "Invalid sudoku") sudokuResult
  putStrLn "Parsed sudoku:"
  print sudoku'
  let solution = solve sudoku'
  maybe (putStrLn "No solution found") print solution

-- | Run the nonogram problem.
-- If a file is provided, parse the nonogram from the file.
-- Otherwise, use the default nonogram.
-- Solve the nonogram and print the solution.
runNonogram :: Maybe Text -> IO ()
runNonogram _ = do
  let nonogram = example :: Nonogram
  putStrLn "Example nonogram:"
  print nonogram
  let solution = solve nonogram
  maybe (putStrLn "No solution found") print solution
  
-- | Run the application.
-- Parse the command line arguments and run the appropriate mode.
run :: IO ()
run = do
  args <- getArgs
  mode <- parseArgs args
  case mode of
    Interactive -> runInteractiveMode
    Demo -> runDemoMode
    RunImmediate expr -> showResult $ parse expr
    File file -> runFile file
    Options.Sudoku file -> runSudoku file
    Options.Nonogram file -> runNonogram file

-- | Main function for the SAT solver application.
main :: IO ()
main = run
