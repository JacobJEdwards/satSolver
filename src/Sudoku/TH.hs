{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Sudoku.TH where

import Sudoku (Sudoku, toDIMACS, sudokuTwentyfive)
import SAT.DIMACS qualified
import Language.Haskell.TH
