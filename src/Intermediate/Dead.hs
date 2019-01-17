{-# LANGUAGE RecordWildCards #-}
module Intermediate.Dead where

import Data.Function ((&))
import qualified Data.Set as S

import Intermediate.Flow
import Common
import Quad

type Needs = S.Set Var

eliminate :: Bool -> Graph Block -> [Block]
eliminate rets g = zipWith eliminateBlock (neededness rets g) $ vertices g

eliminateBlock :: Needs -> Block -> Block
eliminateBlock n = fst . foldr (\q (qs, n) -> (if dead q n then qs else q : qs, update q n)) ([], n)

neededness :: Bool -> Graph Block -> [Needs]
neededness rets Graph{..} = fixed step start
  where
    start = map (const S.empty) (init vertices) ++ [if rets then S.singleton retVar else S.empty]
    step ns =
        let ns' = map (uncurry needednessBlock) $ zip vertices ns
         in zipWith (\s ds -> S.unions $ s : map (ns' !!) ds) ns edges

needednessBlock :: Block -> Needs -> Needs
needednessBlock qs n = foldr update n qs

dead :: Quad -> Needs -> Bool
dead q n = not $ case q of
    Assign v (Val (Var v')) | v == v' -> False
    Assign v e                        -> hasEffects e || needed v n
    Exp e                             -> hasEffects e
    CondJump _ _ _ _                  -> True
    _                                 -> True

update :: Quad -> Needs -> Needs
update q n = n & case q of
    Assign v e                       -> if needed v n || hasEffects e then need (vars e) else id . kill v
    Store (Ptr b i _) a              -> need $ vars' [Var b, i, a]
    CondJump a _ b _                 -> need $ vars' [a, b]
    Exp e | hasEffects e             -> need $ vars e
    _                                -> id

needed :: Var -> Needs -> Bool
needed = S.member

need :: [Var] -> Needs -> Needs
need = flip $ foldr S.insert

kill :: Var -> Needs -> Needs
kill = S.delete
