{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module TypeLevel.SAT () where

import GHC.TypeLits (ErrorMessage (Text), Nat, TypeError)
import GHC.TypeNats (type (-))

data Expr where
  Or :: Expr -> Expr -> Expr
  And :: Expr -> Expr -> Expr
  Not :: Expr -> Expr
  Var :: Nat -> Expr
  Const :: Bool -> Expr

--
type family AllEnvs (n :: Nat) :: [[Bool]] where
  AllEnvs 0 = '[ '[]]
  AllEnvs n = MapExtend (AllEnvs (n - 1)) -- Add T and F for each variable

-- Extend each environment by adding T and F
type family MapExtend (envs :: [[Bool]]) :: [[Bool]] where
  MapExtend '[] = '[]
  MapExtend (x ': xs) = (True ': x) ': (False ': x) ': MapExtend xs

type family Eval (e :: Expr) (env :: [Bool]) :: Bool where
  Eval (Or e1 e2) env = OrE (Eval e1 env) (Eval e2 env)
  Eval (And e1 e2) env = AndE (Eval e1 env) (Eval e2 env)
  Eval (Not e) env = NotE (Eval e env)
  Eval (Var i) env = Lookup i env
  Eval (Const b) _ = b

type family Lookup (i :: Nat) (env :: [Bool]) :: Bool where
  Lookup 0 (b : bs) = b
  Lookup n (b : bs) = Lookup (n - 1) bs

type family OrE (b1 :: Bool) (b2 :: Bool) :: Bool where
  OrE 'True _ = 'True
  OrE _ 'True = 'True
  OrE 'False 'False = 'False

type family AndE (b1 :: Bool) (b2 :: Bool) :: Bool where
  AndE 'False _ = 'False
  AndE _ 'False = 'False
  AndE 'True 'True = 'True

type family NotE (b :: Bool) :: Bool where
  NotE 'True = 'False
  NotE 'False = 'True

type family IsSatisfiable (e :: Expr) (n :: Nat) :: Bool where
  IsSatisfiable e n = AnySatisfies (AllEnvs n) e

type family AnySatisfies (envs :: [[Bool]]) (e :: Expr) :: Bool where
  AnySatisfies '[] _ = False
  AnySatisfies (e ': es) expr = OrE (Eval expr e) (AnySatisfies es expr)

type ExampleF = Or (Var 4) (Not (Var 4))

type Example = IsSatisfiable ExampleF 2

-- :kind! Example
type family ShowResult (result :: Bool) :: Bool where
  ShowResult True = TypeError ('Text "The formula is satisfiable!")
  ShowResult False = TypeError ('Text "The formula is not satisfiable.")

type ShowExample = ShowResult Example
