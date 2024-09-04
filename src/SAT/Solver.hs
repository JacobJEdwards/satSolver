{-#LANGUAGE ScopedTypeVariables#-}

module SAT.Solver
  ( satisfiable,
    simplify,
    toSimple,
    getSolutions,
    Solution,
    checkValue,
  )
where

import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import SAT.CNF
import SAT.Expr
import SAT.Optimisers

type Solution a = [(a, Bool)]

-- findFreeVariable :: Expr a -> Maybe a
-- findFreeVariable (Var c) = Just c
-- findFreeVariable (Not e) = findFreeVariable e
-- findFreeVariable (And e1 e2) = findFreeVariable e1 <|> findFreeVariable e2
-- findFreeVariable (Or e1 e2) = findFreeVariable e1 <|> findFreeVariable e2
-- findFreeVariable (Val _) = Nothing

-- due to haskell's laziness, this function will return the first free variable it finds
findFreeVariable :: Expr a -> Maybe a
findFreeVariable = listToMaybe . collectLiterals

--substitute :: (Eq a) => a -> Bool -> Expr a -> Expr a
--substitute var val expr = case expr of
--  Var c
--    | c == var -> Val val
--    | otherwise -> Var c
--  Not e' -> Not $ substitute var val e'
--  And e1 e2 -> And (substitute var val e1) (substitute var val e2)
--  Or e1 e2 -> Or (substitute var val e1) (substitute var val e2)
--  Val b -> Val b
substitute :: (Eq a) => a -> Bool -> Expr a -> Expr a
substitute var val expr = expr >>= \c -> if c == var then Val val else Var c
  
simplify :: Expr a -> Expr a
simplify (Var c) = Var c
simplify (Not e) = case simplify e of
  Val b -> Val $ not b
  e' -> Not e'
simplify (Or e1 e2) = case (simplify e1, simplify e2) of
  (Val False, e') -> e'
  (e', Val False) -> e'
  (_, Val True) -> Val True
  (Val True, _) -> Val True
  (e1', e2') -> Or e1' e2'
simplify (And e1 e2) = case (simplify e1, simplify e2) of
  (Val True, e') -> e'
  (e', Val True) -> e'
  (Val False, _) -> Val False
  (_, Val False) -> Val False
  (e1', e2') -> And e1' e2'
simplify (Val b) = Val b


simplifyAndCheck :: Expr a -> Bool
simplifyAndCheck = unVal . simplify

toSimple :: (Ord a) => Expr a -> Expr a
toSimple = literalElimination . toCNF . fst . unitPropagate

getSolutions :: (Ord a, Show a) => Expr a -> Maybe (Solution a)
getSolutions expr = getSolutions' expr' $ Set.toList xs
  where
    (expr', xs) = unitPropagate expr

    getSolutions' :: (Ord a, Show a) => Expr a -> Solution a -> Maybe (Solution a)
    getSolutions' expr'' xs' = case findFreeVariable expr'' of
      Nothing -> if simplifyAndCheck expr'' then Just xs' else Nothing
      Just c ->
        let trueGuess = simplify $ substitute c True expr''
            falseGuess = simplify $ substitute c False expr''
            trueSolution = getSolutions' trueGuess ((c, True) : xs')
            falseSolution = getSolutions' falseGuess ((c, False) : xs')
         in case (trueSolution, falseSolution) of
              (Just sol, _) -> Just sol
              (_, Just sol) -> Just sol
              _ -> Nothing

checkValue :: (Eq a) => Solution a -> a -> Bool
checkValue solution value = (value, True) `elem` solution

satisfiable :: (Ord a, Show a) => Expr a -> Bool
satisfiable expr = case findFreeVariable expr' of
  Nothing -> simplifyAndCheck expr'
  Just c ->
    let trueGuess = simplify $ substitute c True expr'
        falseGuess = simplify $ substitute c False expr'
     in satisfiable trueGuess || satisfiable falseGuess
  where
    expr' = toSimple expr
