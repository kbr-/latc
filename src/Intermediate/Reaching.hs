{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Intermediate.Reaching where

import Control.Arrow
import Control.Exception
import Control.Lens (at, non, (%~))
import Control.Monad.State.Strict
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

import Common
import Quad
import Intermediate.Flow

type Def = (Int, Int)
type Defs = M.Map Var (S.Set Def)

reaching :: Graph Block -> [Defs]
reaching Graph{..} = fixed step start
  where
    start = map (const M.empty) vertices
    step reachingBegins =
        let reachingEnds = zipWith (uncurry reachingBlock) (indexed vertices) reachingBegins
         in map (M.unionsWith S.union . map (reachingEnds !!)) ies
    ies = incoming edges

reachingBlock :: Int -> Block -> Defs -> Defs
reachingBlock block qs begin = fst $ foldl step (begin, 0) qs
  where step (r, l) q = (update block l q r, l + 1)

update :: Int -> Int -> Quad -> Defs -> Defs
update block line = \case
    Assign v _ -> def v (block, line) . kill v
    _          -> id

kill :: Var -> Defs -> Defs
kill = M.delete

def :: Var -> Def -> Defs -> Defs
def v d = at v . non S.empty %~ (S.insert d)
