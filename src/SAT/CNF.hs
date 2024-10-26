{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ImportQualifiedPost #-}

module SAT.CNF (applyLaws, toCNF, CNF(CNF), Clause, Literal(Pos, Neg, Const), toExpr) where

import SAT.Expr (type Expr(Not, And, Or, Val, Var))
import Data.Kind (Type)
import Data.Vector (Vector)
import Data.Vector qualified as V

-- data cnf is list of clauses

-- consider moving these into vector or seq
type CNF :: Type -> Type
newtype CNF a = CNF (Vector (Clause a))
  deriving stock (Eq, Show, Ord, Functor, Foldable, Traversable)

type Clause a = Vector (Literal a)

type Literal :: Type -> Type
data Literal a = Pos a | Neg a | Const Bool
  deriving stock (Eq, Show, Ord, Functor, Foldable, Traversable)

-- https://en.wikipedia.org/wiki/Tseytin_transformation -> look into this

-- https://hackage.haskell.org/package/picologic-0.3.0/docs/src/Picologic-Tseitin.html
deMorgansLaws :: Expr a -> Expr a
deMorgansLaws (Not (Not e)) = deMorgansLaws e
deMorgansLaws (Not (And e1 e2)) = Or (deMorgansLaws $ Not e1) (deMorgansLaws $ Not e2)
deMorgansLaws (Not (Or e1 e2)) = And (deMorgansLaws $ Not e1) (deMorgansLaws $ Not e2)
deMorgansLaws (Not (Val b)) = Val $ not b
deMorgansLaws (And e1 e2) = And (deMorgansLaws e1) (deMorgansLaws e2)
deMorgansLaws (Or e1 e2) = Or (deMorgansLaws e1) (deMorgansLaws e2)
deMorgansLaws (Not e) = Not $ deMorgansLaws e
deMorgansLaws e = e

distributiveLaws :: Expr a -> Expr a
distributiveLaws (Or e1 (And e2 e3)) = And (Or (distributiveLaws e1) (distributiveLaws e2)) (Or (distributiveLaws e1) (distributiveLaws e3))
distributiveLaws (Or (And e1 e2) e3) = And (Or (distributiveLaws e3) (distributiveLaws e1)) (Or (distributiveLaws e3) (distributiveLaws e2))
distributiveLaws (Or e1 e2) = Or (distributiveLaws e1) (distributiveLaws e2)
distributiveLaws (And e1 e2) = And (distributiveLaws e1) (distributiveLaws e2)
distributiveLaws (Not e) = Not $ distributiveLaws e
distributiveLaws e = e

applyLaws :: (Eq a) => Expr a -> Expr a
applyLaws expr
  | expr == expr' = expr
  | otherwise = applyLaws expr'
  where
    expr' = distributiveLaws $ deMorgansLaws expr
{-# INLINEABLE applyLaws #-}


toCNF :: forall a. (Eq a, Show a) => Expr a -> CNF a
toCNF expr = CNF $ toClauses cnf
  where 
    cnf :: Expr a
    cnf = applyLaws expr
    
    toClauses :: Expr a -> Vector (Clause a)
    toClauses (And e1 e2) = toClauses e1 <> toClauses e2
    toClauses e = V.singleton $ toClause e
    
    toClause :: Expr a -> Clause a
    toClause (Or e1 e2) = toClause e1 <> toClause e2
    toClause e = V.singleton $ toLiteral e
    
    toLiteral :: Expr a -> Literal a
    toLiteral (Not (Var n)) = Neg n
    toLiteral (Var n) = Pos n
    toLiteral l = error $ "Invalid literal" ++ show l
{-# INLINEABLE toCNF #-}

toExpr :: forall a. CNF a -> Expr a
toExpr (CNF clauses) = foldr1 And $ V.toList $ V.map toClause clauses
  where
    toClause :: Clause a -> Expr a
    toClause literals = foldr1 Or $ V.toList $ V.map toLiteral literals
    
    toLiteral :: Literal a -> Expr a
    toLiteral (Pos n) = Var n
    toLiteral (Neg n) = Not $ Var n
    toLiteral (Const b) = Val b
{-# INLINEABLE toExpr #-}