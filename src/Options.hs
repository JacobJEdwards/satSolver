module Options (Flag (..), parseArgs) where

import Control.Monad (unless)
import qualified Data.List as List
import Data.String (fromString)
import Data.Text (Text)
import System.Console.GetOpt
import System.Exit (exitFailure)

data Flag = Interactive | Demo | RunImmediate Text | File Text | Sudoku Text | Nonogram Text deriving (Show, Eq)

newtype Options = Options
  { optMode :: Flag
  }
  deriving (Show)

defaultOptions :: Options
defaultOptions =
  Options
    { optMode = Demo
    }

options :: [OptDescr (Options -> Options)]
options =
  [ Option
      ['i']
      ["interactive"]
      (NoArg (\opts -> opts {optMode = Interactive}))
      "Run in interactive mode",
    Option
      ['d']
      ["demo"]
      (NoArg (\opts -> opts {optMode = Demo}))
      "Run in default demo mode (default)",
    Option
      ['r']
      ["run"]
      (ReqArg (\arg opts -> opts {optMode = RunImmediate $ fromString arg}) "EXPR")
      "Run the given expression",
    Option
      ['f']
      ["file"]
      (ReqArg (\arg opts -> opts {optMode = File $ fromString arg}) "FILE")
      "Run the given file",
    Option
      ['s']
      ["sudoku"]
      (ReqArg (\arg opts -> opts {optMode = Sudoku $ fromString arg}) "FILE")
      "Run sudoku",
    Option
      ['n']
      ["nonogram"]
      (ReqArg (\arg opts -> opts {optMode = Nonogram $ fromString arg}) "FILE")
      "Run nonogram"
  ]

parseArgs :: [String] -> IO Flag
parseArgs args = do
  let (opts, _, errs) = getOpt Permute options args
  unless (List.null errs) $ do
    mapM_ putStrLn errs
    exitFailure
  return $ optMode $ foldl (flip id) defaultOptions opts
