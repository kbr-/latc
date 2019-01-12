{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
module Intermediate.Liveness where

import Control.Arrow
import Control.Exception
import Control.Lens
import Control.Monad.State.Strict
import qualified Data.Map as M
import qualified Data.Set as S

import Common
import Quad
import Intermediate.Flow

data Use = Use
    { _nextUse    :: Int
    , _reachesEnd :: Bool
    , _passesCall :: Bool
    }

type Uses = M.Map Var Use

makeLenses ''Use

liveness :: Bool -> Graph Block -> [S.Set Var]
liveness rets Graph{..} = fixed step start
  where
    start = map (const S.empty) (init vertices) ++ [if rets then S.singleton retVar else S.empty]
    step aliveEnds =
        let aliveBegin = map (snd . uncurry nextUses) $ zip vertices aliveEnds
         in zipWith (\s dests -> S.unions $ s : map (\i -> assert (i < length aliveBegin) $ aliveBegin !! i) dests) aliveEnds edges

nextUses :: [Quad] -> S.Set Var -> ([Uses], S.Set Var)
nextUses qs aliveEnd = (reverse *** (M.keysSet . fst)) . flip runState start . forM (reverse qs) $ \q -> do
    (u, i) <- get
    let u' = if isCall q then markCall u else u
    put $ (update i q u', i-1)
    pure u
  where
    update :: Int -> Quad -> Uses -> Uses
    update i = \case
        Assign v e         -> flip (foldr $ useVar i) (vars e) . kill v
        CondJump a1 _ a2 _ -> flip (foldr $ useVar i) $ vars' [a1, a2]
        Exp e              -> flip (foldr $ useVar i) $ vars e
        _                  -> id

    kill :: Var -> Uses -> Uses
    kill = M.delete

    isCall :: Quad -> Bool
    isCall = \case
        Assign _ (Call _ _) -> True
        Exp (Call _ _)      -> True
        _                   -> False

    markCall :: Uses -> Uses
    markCall = M.map $ passesCall .~ True

    start :: (Uses, Int)
    start = (M.fromSet (const $ Use (cnt + 1) True False) aliveEnd, cnt)

    cnt :: Int
    cnt = length qs

useVar :: Int -> Var -> Uses -> Uses
useVar i v = at v %~ maybe (Just $ Use i False False) (Just . (nextUse .~ i))
