module Lib
  ( satisfiable,
    simplify,
    toSimple,
    getSolutions,
    module Expr,
    module Parser.Parser,
    Solution (..),
  )
where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Expr
import Parser.Parser

collectLiterals :: (Ord a) => Expr a -> Set a
collectLiterals (Var v) = Set.singleton v
collectLiterals (And e1 e2) = Set.union (collectLiterals e1) (collectLiterals e2)
collectLiterals (Or e1 e2) = Set.union (collectLiterals e1) (collectLiterals e2)
collectLiterals (Not e) = collectLiterals e
collectLiterals _ = Set.empty

literalPolarity :: a -> Expr a -> Maybe Polarity
literalPolarity c (Var v)
  | c == v = Just Positive
  | otherwise = Nothing
literalPolarity c (Not (Var v))
  | c == v = Just Negative
  | otherwise = Nothing
literalPolarity c (And e1 e2) = literalPolarity c e1 <> literalPolarity c e2
literalPolarity c (Or e1 e2) = literalPolarity c e1 <> literalPolarity c e2
literalPolarity c (Not e) = flipPolarity <$> literalPolarity c e
literalPolarity _ _ = Nothing

eliminateLiteral :: a -> Expr a -> Polarity -> Expr a
eliminateLiteral c e@(Var v) p
  | c == v && p == Positive = Const True
  | c == v && p == Negative = Const False
  | otherwise = e
eliminateLiteral c e@(Not (Var v)) p
  | c == v && p == Positive = Const False
  | c == v && p == Negative = Const True
  | otherwise = e
eliminateLiteral c (And e1 e2) p = And (eliminateLiteral c e1 p) (eliminateLiteral c e2 p)
eliminateLiteral c (Or e1 e2) p = Or (eliminateLiteral c e1 p) (eliminateLiteral c e2 p)
eliminateLiteral c (Not e) p = Not $ eliminateLiteral c e $ flipPolarity p
eliminateLiteral _ e _ = e

eliminateLiterals :: Expr a -> Set a -> Expr a
eliminateLiterals = foldr eliminate
  where
    eliminate c e' = eliminateLiteral c e' $ fromMaybe (error "Literal not found") (literalPolarity c e')

literalElimination :: (Ord a) => Expr a -> Expr a
literalElimination e = eliminateLiterals e $ collectLiterals e

unitClause :: Expr a -> Maybe (a, Bool)
unitClause (Var c) = Just (c, True)
unitClause (Not (Var c)) = Just (c, False)
unitClause _ = Nothing

clauses :: Expr a -> [Expr a]
clauses (And e1 e2) = clauses e1 <> clauses e2
clauses e = [e]

allUnitClauses :: Expr a -> [(a, Bool)]
allUnitClauses = mapMaybe unitClause . clauses

unitPropagate :: Expr a -> Expr a
unitPropagate e = case allUnitClauses e of
  [] -> e
  (c, b) : _ -> unitPropagate $ eliminateLiteral c e $ if b then Positive else Negative

findFreeVariable :: Expr a -> Maybe a
findFreeVariable (Var c) = Just c
findFreeVariable (Not e) = findFreeVariable e
findFreeVariable (And e1 e2) = findFreeVariable e1 <|> findFreeVariable e2
findFreeVariable (Or e1 e2) = findFreeVariable e1 <|> findFreeVariable e2
findFreeVariable (Const _) = Nothing

substitute :: a -> Bool -> Expr a -> Expr a
substitute var val e = case e of
  Var c
    | c == var -> Const val
    | otherwise -> Var c
  Not e' -> Not $ substitute var val e'
  And e1 e2 -> And (substitute var val e1) (substitute var val e2)
  Or e1 e2 -> Or (substitute var val e1) (substitute var val e2)
  Const b -> Const b

simplify :: Expr a -> Expr a
simplify (Var c) = Var c
simplify (Not e) = case simplify e of
  Const b -> Const $ not b
  e' -> Not e'
simplify (Or e1 e2) = case (simplify e1, simplify e2) of
      (Const False, e') -> e'
      (e', Const False) -> e'
      (_, Const True) -> Const True
      (Const True, _) -> Const True
      (e1'', e2'') -> Or e1'' e2''
simplify (And e1 e2) = case (simplify e1, simplify e2) of
      (Const True, e') -> e'
      (e', Const True) -> e'
      (Const False, _) -> Const False
      (_, Const False) -> Const False
      (e1'', e2'') -> And e1'' e2''
simplify (Const b) = Const b

unConst :: Expr a -> Bool
unConst (Const b) = b
unConst _ = error "Non const type"

simplifyAndCheck :: Expr a -> Bool
simplifyAndCheck = unConst . simplify

toSimple :: (Ord a) => Expr a -> Expr a
toSimple = literalElimination . toCNF . unitPropagate

data Solution a = Solved [(a, Bool)] | No deriving (Show)

getSolutions' :: (Ord a) => Expr a -> [(a, Bool)] -> Solution a
getSolutions' expr xs = case findFreeVariable expr of
   Nothing -> if simplifyAndCheck expr then Solved xs else No
   Just c -> 
      let trueGuess = simplify $ substitute c True expr
          falseGuess = simplify $ substitute c False expr
          trueSolution = getSolutions' trueGuess ((c, True):xs)
          falseSolution = getSolutions' falseGuess ((c, False):xs)
      in case (trueSolution, falseSolution) of 
        (Solved sol, _) -> Solved sol
        (_, Solved sol) -> Solved sol
        _ -> No


getSolutions :: (Ord a) => Expr a -> Solution a
getSolutions expr = case getSolutions' expr [] of 
  Solved xs -> Solved $ reverse xs
  No -> No

satisfiable :: (Ord a) => Expr a -> Bool
satisfiable expr = case findFreeVariable expr' of
  Nothing -> simplifyAndCheck expr'
  Just c ->
    let trueGuess = simplify $ substitute c True expr'
        falseGuess = simplify $ substitute c False expr'
     in satisfiable trueGuess || satisfiable falseGuess
  where
    expr' = toSimple expr
