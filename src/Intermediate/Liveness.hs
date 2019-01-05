{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
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
    , _lastUse    :: Int
    , _passesCall :: Bool
    }

makeLenses ''Use

liveness :: Bool -> Graph Block -> [S.Set Var]
liveness rets Graph{..} = fixed step start
  where
    start = map (const S.empty) (init vertices) ++ [if rets then S.singleton retVar else S.empty]
    step aliveEnds =
        let aliveBegin = map (snd . uncurry nextUses) $ zip vertices aliveEnds
         in zipWith (\s dests -> S.unions $ s : map (\i -> assert (i < length aliveBegin) $ aliveBegin !! i) dests) aliveEnds edges

nextUses :: [Quad] -> S.Set Var -> ([(Quad, M.Map Var Use)], S.Set Var)
nextUses qs aliveEnd = (reverse *** (M.keysSet . fst)) . flip runState start . forM (reverse qs) $ \q -> do
    (u, i) <- get
    let u' = if isCall q then markCall u else u
    put $ (update i q u', i-1)
    pure (q, u)
  where
    update :: Int -> Quad -> M.Map Var Use -> M.Map Var Use
    update i = \case
        Assign v e         -> flip (foldr $ use i) (vars e) . kill v
        CondJump a1 _ a2 _ -> flip (foldr $ use i) $ vars' [a1, a2]
        Exp e              -> flip (foldr $ use i) $ vars e
        _                    -> id

    use :: Int -> Var -> M.Map Var Use -> M.Map Var Use
    use i v = at v %~ maybe (Just $ Use i i False) (Just . (nextUse .~ i))

    kill :: Var -> M.Map Var Use -> M.Map Var Use
    kill = M.delete

    isCall :: Quad -> Bool
    isCall = \case
        Assign _ (Call _ _) -> True
        Exp (Call _ _)      -> True
        _                       -> False

    markCall :: M.Map Var Use -> M.Map Var Use
    markCall = M.map $ passesCall .~ True

    start :: (M.Map Var Use, Int)
    start = (M.fromSet (const $ Use (cnt + 1) (cnt + 1) False) aliveEnd, cnt)

    cnt :: Int
    cnt = length qs
