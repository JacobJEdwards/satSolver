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
import Problem (example, parse, parseFile, solve, toCNF)
import SAT (type Expr (And, Not, Or, Var))
import SAT qualified
import SAT.DIMACS qualified as DIMACS
import Sudoku (type Sudoku)
import System.Console.Haskeline (defaultSettings, getInputLine, outputStrLn, runInputT, type InputT)
import System.Environment (getArgs)

showResult :: (Show a, Ord a) => Maybe (Expr a) -> IO ()
showResult = maybe (putStrLn "Failed to parse expression") showExprInfo

showExprInfo :: (Show a, Ord a) => Expr a -> IO ()
showExprInfo expr = do
  let cnf = SAT.toCNF expr
  putStrLn $ "CNF: " <> show cnf
  let simplified = SAT.toSimple cnf
  putStrLn $ "Simplified step 1: " <> show simplified
  let simplified' = SAT.propagateValue simplified
  putStrLn $ "Propagated: " <> show simplified'
  let satisfiable' = SAT.satisfiable expr
  putStrLn $ "Satisfiable: " <> show satisfiable'
  let solutions = SAT.getSolutions expr
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
          let result = parse $ fromString input :: Maybe (Expr Int)
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

runNonogram :: Maybe Text -> IO ()
runNonogram _ = do
  let nonogram = example :: Nonogram
  putStrLn "Example nonogram:"
  print nonogram
  let solution = solve nonogram
  maybe (putStrLn "No solution found") print solution
  
run :: IO ()
run = do
  args <- getArgs
  mode <- parseArgs args
  case mode of
    Interactive -> runInteractiveMode
    Demo -> runDemoMode
    RunImmediate expr -> showResult (parse expr :: Maybe (Expr Int))
    File file -> runFile file
    Options.Sudoku file -> runSudoku file
    Options.Nonogram file -> runNonogram file

main :: IO ()
main = run
