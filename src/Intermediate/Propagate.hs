{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Intermediate.Propagate where

import Extra (allSame)
import Data.Proxy
import Control.Lens
import Control.Exception
import Control.Monad.State.Strict
import qualified Data.Map as M
import qualified Data.Set as S

import Common
import Quad
import Intermediate.Flow
import Intermediate.Reaching (Def, Defs)

class ToExp e where
    toExp :: e -> Exp

class (Eq a, Eq (E a), ToExp (E a)) => Propagable a where
    type E a :: *
    update   :: Quad -> a -> (Quad, a)
    insert   :: Var -> (E a) -> a -> a
    holds    :: a -> M.Map Var (E a)
    empty    :: a

instance ToExp Exp where
    toExp = id

propagate :: forall a. Propagable a => Graph (Block, Defs) -> [Block]
propagate Graph{..} = map fst $ fixed step start
  where
    start = map ((, empty) . fst) vertices
    step xs =
        let (bs', csEnds) = unzip $ map (uncurry $ propagateBlock @a) xs
         in zipWith3 (\b defs sources -> (b, merge bs' defs $ map (csEnds !!) sources)) bs' defss ies
    defss = map snd vertices
    ies = incoming edges

propagateBlock :: Propagable a => Block -> a -> (Block, a)
propagateBlock qs = runState $ mapM (\q -> state $ update q) qs

merge :: Propagable a => [Block] -> Defs -> [a] -> a
merge bs defs xs = foldr
    (\v -> case do
        es <- traverse (M.lookup v . holds) xs
        guard $ allSame es
        let ds = defs ^. at v . non S.empty . to S.toList
            d = assert (not $ null ds) $ head ds
        guard $ null $ tail ds
        let e = assert (not $ null es) $ head es
        guard $ bs !! fst d !! snd d == Assign v (toExp e)
        pure e
     of Nothing -> id
        Just e  -> insert v e
    ) empty $ M.keys $ M.unions $ map holds xs
