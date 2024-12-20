{-# LANGUAGE Safe #-}
{-# LANGUAGE Strict #-}

-- |
-- Module      : Parser
-- Description : Exports the parser module.
module Parser
  ( module Parser.Parsec,
    module Parser.Error,
    module Parser.Result,
    module Parser.Input,
  )
where

import Parser.Error
import Parser.Input
import Parser.Parsec
import Parser.Result
