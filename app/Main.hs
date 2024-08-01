module Main (main) where

import Lib

main :: IO ()
main = do
  print $ And (Var 'x') ((Var 'x'))
  print $ satisfiable $ And (Var 'x') ((Var 'x'))
