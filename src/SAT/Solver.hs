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
-- findFreeVariable (Const _) = Nothing

findFreeVariable :: Expr a -> Maybe a
findFreeVariable = listToMaybe . collectLiterals

substitute :: (Eq a) => a -> Bool -> Expr a -> Expr a
substitute var val expr = case expr of
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
  (e1', e2') -> Or e1' e2'
simplify (And e1 e2) = case (simplify e1, simplify e2) of
  (Const True, e') -> e'
  (e', Const True) -> e'
  (Const False, _) -> Const False
  (_, Const False) -> Const False
  (e1', e2') -> And e1' e2'
simplify (Const b) = Const b

unConst :: Expr a -> Bool
unConst (Const b) = b
unConst _ = error "Not a constant"

simplifyAndCheck :: Expr a -> Bool
simplifyAndCheck = unConst . simplify

toSimple :: (Ord a) => Expr a -> Expr a
toSimple = literalElimination . toCNF . fst . unitPropagate

getSolutions :: (Ord a, Show a) => Expr a -> Maybe (Solution a)
getSolutions expr = case getSolutions' expr' (Set.toList xs) of
  Just xs' -> Just $ reverse xs'
  Nothing -> Nothing
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

checkValue :: (Eq a) => [(a, Bool)] -> a -> Bool
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
