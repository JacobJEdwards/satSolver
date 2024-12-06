{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-|
Module      : Sudoku
Description : Exports the Sudoku solver and parser modules.
-}

module Sudoku
  ( module Sudoku.Solver,
    module Sudoku.Parser,
  )
where

import Sudoku.Parser
import Sudoku.Solver
