{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Strict #-}

-- |
-- Module      : Options
-- Description : Exports the command line options parser module.
module Options (type Flag (..), parseArgs) where

import Control.Monad (unless)
import Data.Data (type Data)
import Data.Kind (type Type)
import Data.List qualified as List
import Data.String (fromString)
import Data.Text (type Text)
import System.Console.GetOpt (getOpt, type ArgDescr (NoArg, OptArg, ReqArg), type ArgOrder (Permute), type OptDescr (Option))
import System.Exit (exitFailure)

-- | Command line options.
type Flag :: Type
data Flag :: Type where
  -- | Run in interactive mode.
  Interactive :: Flag
  -- | Run in default demo mode.
  Demo :: Flag
  -- | Run the given expression.
  RunImmediate :: Text -> Flag
  -- | Run the given file.
  File :: Text -> Flag
  -- | Run sudoku.
  Sudoku :: Maybe Text -> Flag
  -- | Run nonogram.
  Nonogram :: Maybe Text -> Flag

deriving stock instance Show Flag

deriving stock instance Eq Flag

deriving stock instance Ord Flag

deriving stock instance Data Flag

-- | Command line options.
type Options :: Type
newtype Options = Options
  { -- | The mode to run in.
    optMode :: Flag
  }
  deriving stock (Show)

-- | Default command line options.
defaultOptions :: Options
defaultOptions =
  Options
    { optMode = Nonogram Nothing
    }

-- | Command line options.
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
      (OptArg (\arg opts -> opts {optMode = Sudoku $ fmap fromString arg}) "FILE")
      "Run sudoku",
    Option
      ['n']
      ["nonogram"]
      (OptArg (\arg opts -> opts {optMode = Nonogram $ fmap fromString arg}) "FILE")
      "Run nonogram"
  ]

-- | Parse command line arguments.
-- Returns the parsed command line options.
--
-- >>> parseArgs ["-i"]
-- Interactive
--
-- >>> parseArgs ["-d"]
-- Demo
parseArgs :: [String] -> IO Flag
parseArgs args = do
  let (opts, _, errs) = getOpt Permute options args
  unless (List.null errs) $ do
    mapM_ putStrLn errs
    exitFailure
  return $ optMode $ foldl (flip id) defaultOptions opts
