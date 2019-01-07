{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
module Intermediate.CSE where

import Data.Maybe
import Data.List ((\\))
import Control.Lens
import Control.Exception
import Control.Monad.State.Strict
import Extra (allSame)
import qualified Data.Map as M
import qualified Data.Set as S

import Common
import Quad
import Intermediate.Flow
import Intermediate.Reaching (Def, Defs)

data CS = CS
    { holds  :: M.Map Var Exp
    , heldIn :: M.Map Exp Var
    , occurs :: M.Map Var [Exp]
    }
    deriving Eq

eliminate :: Graph (Block, Defs) -> [Block]
eliminate Graph{..} = map fst $ fixed step start
  where
    start = map ((, empty) . fst) vertices
    step xs =
        let (bs', csEnds) = unzip $ map (uncurry eliminateBlock) xs
         in zipWith (\(i, b) sources -> (b, merge i bs' sources csEnds)) (Common.indexed bs') ies
    merge i bs sources css = foldr
        (\e -> case do
            vs <- traverse (M.lookup e . heldIn . (css !!)) sources
            guard $ allSame vs
            let v = assert (not $ null vs) $ head vs
                ds = (defs !! i) ^. at v . non S.empty . to S.toList
                d = assert (not $ null ds) $ head ds
            guard $ null $ tail ds
            guard $ bs !! fst d !! snd d == Assign v e
            pure v
         of Nothing -> id
            Just v  -> insert v e
        ) empty $ M.keys $ M.unions $ map (heldIn . (css !!)) $ sources
    defs = map snd vertices
    ies = incoming edges

empty :: CS
empty = CS M.empty M.empty M.empty

eliminateBlock :: Block -> CS -> (Block, CS)
eliminateBlock qs = runState $ mapM (\q -> state $ update q) qs

update :: Quad -> CS -> (Quad, CS)
update q@(Assign v e@(BinInt _ _ _)) cs@CS{..} = (q', cs')
  where
    q' = maybe q (Assign v . Val . Var) curr
    cs' = if | curr == Just v                 -> cs
             | isJust curr || elem v (vars e) -> delete v cs
             | otherwise                      -> insert v e . delete v $ cs
    curr = heldIn ^. at e
update q@(Assign v _) cs = (q, delete v cs)
update q cs = (q, cs)

delete :: Var -> CS -> CS
delete v CS{..} = CS holds' heldIn' occurs'
  where
    holds'  = M.delete v holds
    heldIn' = foldr M.delete heldIn $ occurs ^. at v . non [] ++ heldInV
    occurs' = foldr (\x -> ix x %~ (\\ heldInV)) (M.delete v occurs) $ concatMap vars heldInV
    heldInV = catMaybes $ [holds ^. at v]

insert :: Var -> Exp -> CS -> CS
insert v e CS{..} = CS holds' heldIn' occurs'
  where
    holds'  = M.insert v e holds
    heldIn' = M.insert e v heldIn
    occurs' = foldr (\x o -> o & at x . non [] %~ (e :)) occurs $ vars e
