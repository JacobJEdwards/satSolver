{-|
Module      : SAT.DIMACS
Description : Re-exports the DIMACS module.
-}

{-# LANGUAGE Safe #-}

module SAT.DIMACS
  ( module SAT.DIMACS.CNF,
    module SAT.DIMACS.Parser,
  )
where

import SAT.DIMACS.CNF
import SAT.DIMACS.Parser
