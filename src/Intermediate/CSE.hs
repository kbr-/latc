{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
module Intermediate.CSE where

import Data.Maybe
import Data.List ((\\))
import Control.Lens
import Control.Monad.State.Strict
import qualified Data.Map as M

import Common
import Quad
import Intermediate.Flow
import Intermediate.Reaching (Defs)
import qualified Intermediate.Propagate as P

data CS = CS
    { holds  :: M.Map Var Exp
    , heldIn :: M.Map Exp Var
    , occurs :: M.Map Var [Exp]
    }
    deriving Eq

instance P.Propagable CS where
    type E CS = Exp
    update    = update
    insert    = insert
    holds     = holds
    empty     = CS M.empty M.empty M.empty

eliminate :: Graph Block -> [Defs] -> [Block]
eliminate = P.propagate @CS

update :: Quad -> CS -> (Quad, CS)
update q@(Assign v e) cs@CS{..} | applicable e = (q', cs')
  where
    q' = maybe q (Assign v . Val . Var) curr
    cs' = if | curr == Just v                 -> cs
             | isJust curr || elem v (vars e) -> delete v cs
             | otherwise                      -> insert v e . delete v $ cs
    curr = heldIn ^. at e
update q@(Assign v _) cs = (q, delete v cs)
update q cs = (q, cs)

applicable :: Exp -> Bool
applicable = \case
    BinInt _ _ _ -> True
    Load _       -> True
    _            -> False

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
