{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module SAT.Expr.Quoter (
    expr
) where

import SAT.Expr.Parser(parse)
import Language.Haskell.TH.Quote (QuasiQuoter (QuasiQuoter, quoteExp, quotePat, quoteType, quoteDec), dataToExpQ)
import Data.Text (pack)


expr :: QuasiQuoter
expr = QuasiQuoter {
    quoteExp = \str -> do 
        case parse (pack str) of 
            Just e -> dataToExpQ (const Nothing) e
            Nothing -> fail "Invalid expression"
    , quotePat = undefined
    , quoteType = undefined
    , quoteDec = undefined
}