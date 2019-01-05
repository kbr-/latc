{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Intermediate.Flow where

import Data.List (nub)
import Control.Exception
import qualified Data.Map as M

import Common
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

mapGraph :: (a -> b) -> Graph a -> Graph b
mapGraph f Graph{..} = Graph (map f vertices) edges

splitBlocks :: [Quad] -> [Block]
splitBlocks qs = reverse . map reverse . fst . foldl step ([[]], False) $ qs
  where
    step :: ([Block], Bool) -> Quad -> ([Block], Bool)
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

incoming :: [[Int]] -> [[Int]]
incoming edges = map ((\i -> map fst . filter (\(_, es) -> elem i es) $ ies) . fst) ies
  where ies = indexed edges

edges' :: [Block] -> [[Int]]
edges' bs = map dests ibs
  where
    dests :: (Int, Block) -> [Int]
    dests (i, b) = case assert (not $ null b) $ last b of
        Jump l           -> [labelIx l]
        CondJump _ _ _ l -> nub $ (if i < cnt - 1 then ((i+1) :) else id) [labelIx l]
        _                -> if i < cnt - 1 then [i + 1] else []

    labelIx :: Label -> Int
    labelIx l = assert (M.member l labels) $ labels M.! l

    labels :: M.Map Label Int
    labels = foldr (\(i, b) m ->
        case assert (not $ null b) $ head b of
            Mark l -> M.insert l i m
            _      -> m) M.empty ibs

    ibs :: [(Int, Block)]
    ibs = indexed bs

    cnt :: Int
    cnt = length bs
