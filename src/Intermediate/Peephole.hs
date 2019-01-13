{-# LANGUAGE LambdaCase #-}
module Intermediate.Peephole where

import Quad

type Pattern = [Quad] -> Maybe [Quad]

patterns = [jumpToNext]

peephole :: [Quad] -> [Quad]
peephole l@(q:qs) = case applyPatterns patterns l of
    Just qs' -> peephole qs'
    Nothing  -> q : peephole qs
peephole [] = []

applyPatterns :: [Pattern] -> [Quad] -> Maybe [Quad]
applyPatterns (p:ps) qs = case p qs of
    Nothing  -> applyPatterns ps qs
    qs'      -> qs'
applyPatterns [] _ = Nothing

jumpToNext :: Pattern
jumpToNext = \case
    Jump l : Mark l' : xs | l == l'           -> Just $ Mark l : xs
    CondJump _ _ _ l : Mark l' : xs | l == l' -> Just $ Mark l : xs
    _                                         -> Nothing
