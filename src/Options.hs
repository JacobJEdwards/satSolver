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

module Options (type Flag (..), parseArgs) where

import Control.Monad (unless)
import Data.Data (type Data)
import Data.Kind (type Type)
import Data.List qualified as List
import Data.String (fromString)
import Data.Text (type Text)
import System.Console.GetOpt (getOpt, type ArgDescr (NoArg, OptArg, ReqArg), type ArgOrder (Permute), type OptDescr (Option))
import System.Exit (exitFailure)

type Flag :: Type
data Flag :: Type where
  Interactive :: Flag
  Demo :: Flag
  RunImmediate :: Text -> Flag
  File :: Text -> Flag
  Sudoku :: Maybe Text -> Flag
  Nonogram :: Maybe Text -> Flag

deriving stock instance Show Flag

deriving stock instance Eq Flag

deriving stock instance Ord Flag

deriving stock instance Data Flag

type Options :: Type
newtype Options = Options
  { optMode :: Flag
  }
  deriving stock (Show)

defaultOptions :: Options
defaultOptions =
  Options
    { optMode = Nonogram Nothing
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
      (OptArg (\arg opts -> opts {optMode = Sudoku $ fmap fromString arg}) "FILE")
      "Run sudoku",
    Option
      ['n']
      ["nonogram"]
      (OptArg (\arg opts -> opts {optMode = Nonogram $ fmap fromString arg}) "FILE")
      "Run nonogram"
  ]

parseArgs :: [String] -> IO Flag
parseArgs args = do
  let (opts, _, errs) = getOpt Permute options args
  unless (List.null errs) $ do
    mapM_ putStrLn errs
    exitFailure
  return $ optMode $ foldl (flip id) defaultOptions opts
