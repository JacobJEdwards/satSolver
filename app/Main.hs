module Main (main) where

import Lib
import Control.Monad (unless)
import System.Environment (getArgs)
import System.Console.GetOpt
import System.Exit (exitFailure)
import System.Console.Haskeline
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.String(fromString)
import qualified Data.List as List
import Parser.Parsec
import qualified Parser.DIMACS as DIMACS
import Sudoku

data Flag = Interactive | Demo | RunImmediate Text | File Text | Sudoku Text deriving (Show, Eq)  

newtype Options = Options {
  optMode :: Flag
} deriving (Show)

defaultOptions :: Options
defaultOptions = Options {
  optMode = Demo
}

options :: [OptDescr (Options -> Options)]
options = 
  [ Option ['i'] ["interactive"]
    (NoArg (\opts -> opts { optMode = Interactive }))
    "Run in interactive mode"
  , Option ['d'] ["demo"]
    (NoArg (\opts -> opts { optMode = Demo }))
    "Run in default demo mode (default)"
  , Option ['r'] ["run"]
    (ReqArg (\arg opts -> opts { optMode = RunImmediate $ fromString arg }) "EXPR")
    "Run the given expression immediately"
  , Option ['f'] ["file"]
    (ReqArg (\arg opts -> opts { optMode = File $ fromString arg }) "FILE")
    "Run the given file immediately"
  , Option ['s'] ["sudoku"]
    (ReqArg (\arg opts -> opts { optMode = Sudoku $ fromString arg }) "FILE")
    "Run the sudoku example immediately"
  ]

parseArgs :: [String] -> IO Options
parseArgs args = do
  let (opts, _, errs) = getOpt Permute options args
  unless (List.null errs) $ do
    mapM_ putStrLn errs
    exitFailure
  return $ foldl (flip id) defaultOptions opts
  

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
          let result = Lib.parse $ fromString input
          liftIO $ showResult result
          loop


runDemoMode :: IO ()
runDemoMode = do
  let expr = And (Var 'A') (Or (Var 'B') (Not (Var 'C')))
  putStrLn $ "Demo expression: " <> show expr
  showExprInfo expr
      

run :: IO ()
run = do 
  args <- getArgs
  mode <- parseArgs args
  case optMode mode of 
    Interactive -> runInteractiveMode
    Demo -> runDemoMode
    RunImmediate expr -> showResult $ Lib.parse expr
    File file -> do
      contents <- readFile $ Text.unpack file
      let cnf = DIMACS.parseCNF $ Text.pack contents
      putStrLn $ "Parsed CNF: " <> show cnf
      case cnf of 
        Result (_, cnf') -> showExprInfo (DIMACS.toExpr (DIMACS.clauses cnf'))
        Errors errs -> putStrLn $ "Errors: " <> show errs
    Sudoku _ -> do
      let cnf = sudokuToCNF sudoku
      let clauses = DIMACS.clauses cnf
      let expr = DIMACS.toExpr clauses
      let solutions = getSolutions expr
      case solutions of 
        Solved xs -> do 
           let decoded = decodeSolution xs
           prettyPrint decoded
        No -> putStrLn "No solutions found"
  

main :: IO ()
main = run
  