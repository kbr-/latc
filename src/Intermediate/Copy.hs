{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Intermediate.Copy where

import Control.Lens
import Data.Maybe
import qualified Data.Map as M

import Intermediate.Flow
import Intermediate.Reaching (Defs)
import qualified Intermediate.Propagate as P
import Quad

newtype Vars = Vars (M.Map Var Var)
    deriving Eq

instance P.ToExp Var where
    toExp = toExp

instance P.Propagable Vars where
    type E Vars          = Var
    update               = update
    insert v e (Vars vs) = Vars $ vs & at v ?~ e
    holds (Vars vs)      = vs
    empty                = Vars M.empty

copy :: Graph (Block, Defs) -> [Block]
copy = P.propagate @Vars

update :: Quad -> Vars -> (Quad, Vars)
update q (Vars vs) = (q, ) $ Vars $ case q' of
    Assign v (Val (Var v')) -> vs & at v ?~ v'
    Assign v _              -> vs & at v .~ Nothing
    _                       -> vs
  where
    q' = propQuad (Vars vs) q

propQuad :: Vars -> Quad -> Quad
propQuad vs = \case
    Assign v e        -> Assign v $ propExp vs e
    CondJump a op b l -> CondJump (propArg vs a) op (propArg vs b) l
    Exp e             -> Exp $ propExp vs e
    q                 -> q

propExp :: Vars -> Exp -> Exp
propExp vs = \case
    BinInt a op b -> BinInt (propArg vs a) op (propArg vs b)
    Val a         -> Val (propArg vs a)
    Call f as     -> Call f (map (propArg vs) as)
    e             -> e

propArg :: Vars -> Arg -> Arg
propArg (Vars vs) (Var v) = maybe (Var v) Var $ vs ^. at v
propArg _ x               = x

toExp :: Var -> Exp
toExp = Val . Var
