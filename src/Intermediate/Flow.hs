{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Intermediate.Flow where

import Control.Exception
import qualified Data.Map as M

import Quad

data Graph a = Graph
    { vertices :: [a]
    , edges    :: [[Int]]
    }
    deriving Show

type Block = [Quad]

mkGraph :: [Quad] -> Graph Block
mkGraph qs = Graph{..}
  where
    vertices = splitBlocks qs
    edges = edges' vertices

splitBlocks :: [Quad] -> [[Quad]]
splitBlocks qs = reverse . map reverse . fst . foldl step ([[]], False) $ qs
  where
    step :: ([[Quad]], Bool) -> Quad -> ([[Quad]], Bool)
    step (b:t, jumped) q = (if jumped || jumpedTo q then [q]:b:t else (q:b):t, isJump q)

    isJump = \case
        Jump _           -> True
        CondJump _ _ _ _ -> True
        _                -> False

    jumpedTo = \case
        Mark l -> any (jumpsTo l) qs
        _      -> False

    jumpsTo l = \case
        Jump l           -> True
        CondJump _ _ _ l -> True
        _                -> False

edges' :: [[Quad]] -> [[Int]]
edges' bs = map dests ibs
  where
    dests :: ([Quad], Int) -> [Int]
    dests (b, i) = (if i < cnt - 1 then ((i+1) :) else id) $ jumpDests b

    jumpDests :: [Quad] -> [Int]
    jumpDests b = case assert (not $ null b) $ last b of
        Jump l           -> [labelIx l]
        CondJump _ _ _ l -> [labelIx l]
        _                -> []

    labelIx :: Label -> Int
    labelIx l = assert (M.member l labels) $ labels M.! l

    labels :: M.Map Label Int
    labels = foldr (\(b, i) m ->
        case assert (not $ null b) $ head b of
            Mark l -> M.insert l i m
            _      -> m) M.empty ibs

    ibs :: [([Quad], Int)]
    ibs = reverse . fst . foldl (\(xs, i) x -> ((x,i):xs, i+1)) ([], 0) $ bs

    cnt :: Int
    cnt = length bs
