module Lib (
  Expr(..),
  satisfiable,
 ) where

import Control.Applicative((<|>))

data Expr = Var Char
            | Not Expr
            | And Expr Expr
            | Or Expr Expr
            | Const Bool
    deriving (Eq)


instance Show Expr where
  show (Var c) = [c]
  show (Not e) = "¬" ++ show e
  show (And e1 e2) = "(" ++ show e1 ++ " ∧ " ++ show e2 ++ ")"
  show (Or e1 e2) = "(" ++ show e1 ++ " ∨ " ++ show e2 ++ ")"
  show (Const b) = show b


findFreeVariable :: Expr -> Maybe Char
findFreeVariable (Var c) = Just c
findFreeVariable (Not e) = findFreeVariable e
findFreeVariable (And e1 e2) = findFreeVariable e1 <|> findFreeVariable e2
findFreeVariable (Or e1 e2) = findFreeVariable e1 <|> findFreeVariable e2
findFreeVariable(Const _) = Nothing


substitute :: Char -> Bool -> Expr -> Expr
substitute var val e = case e of
    Var c -> if c == var then Const val else Var c
    Not e' -> Not $ substitute var val e'
    And e1 e2 -> And (substitute var val e1) (substitute var val e2)
    Or e1 e2 -> Or (substitute var val e1) (substitute var val e2)
    Const b -> Const b


simplify :: Expr -> Expr
simplify (Var c) = Var c
simplify (Not e) = case simplify e of
    Const b -> Const (not b)
    e' -> Not e'
simplify (Or e1 e2) = case (simplify e1, simplify e2) of
    (Const False, e') -> simplify e'
    (e', Const False) -> simplify e'
    (_, Const True) -> Const True
    (Const True, _) -> Const True
    (e1', e2') -> Or e1' e2'
simplify (And e1 e2) = case (simplify e1, simplify e2) of
    (Const True, e') -> simplify e'
    (e', Const True) -> simplify e'
    (Const False, _) -> Const False
    (_, Const False) -> Const False
    (e1', e2') -> And e1' e2'
simplify (Const b) = Const b

unConst :: Expr -> Bool
unConst (Const b) = b
unConst _ = error "Non const type"


satisfiable :: Expr -> Bool
satisfiable expr = case findFreeVariable expr of
   Nothing -> unConst expr
   Just c ->
      let trueGuess = simplify (substitute c True expr)
          falseGuess = simplify (substitute c False expr)
      in satisfiable trueGuess || satisfiable falseGuess


