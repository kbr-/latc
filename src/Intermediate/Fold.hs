{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
module Intermediate.Fold where

import Prelude hiding (LT, GT, EQ)
import Control.Lens
import Data.Maybe
import qualified Data.Map as M

import Common
import Intermediate.Flow
import Intermediate.Reaching (Defs)
import qualified Intermediate.Propagate as P
import Quad hiding (Consts)

newtype Consts = Consts (M.Map Var Integer)
    deriving Eq

instance P.ToExp Integer where
    toExp = toExp

instance P.Propagable Consts where
    type E Consts          = Integer
    update                 = update
    insert v e (Consts cs) = Consts $ cs & at v ?~ e
    holds (Consts cs)      = cs
    empty                  = Consts M.empty

fold :: Graph (Block, Defs) -> [Quad]
fold = clean . concat . P.propagate @Consts

clean :: [Quad] -> [Quad]
clean = mapMaybe cleanQuad

cleanQuad :: Quad -> Maybe Quad
cleanQuad = \case
    CondJump (ConstI a) op (ConstI b) l -> if relFun op a b then Just $ Jump l else Nothing
    Exp e@(Call _ _)                    -> Just $ Exp e
    Exp _                               -> Nothing
    q                                   -> Just q

update :: Quad -> Consts -> (Quad, Consts)
update q (Consts cs) = (q', ) $ Consts $ case q' of
    Assign v (Val (ConstI x)) -> cs & at v ?~ x
    Assign v _                -> cs & at v .~ Nothing
    _                         -> cs
  where
    q' = foldQuad $ propQuad (Consts cs) q

foldQuad :: Quad -> Quad
foldQuad = \case
    Assign v e -> Assign v $ foldExp e
    Exp e      -> Exp $ foldExp e
    q          -> q

foldExp :: Exp -> Exp
foldExp = \case
    BinInt (ConstI a) op (ConstI b) -> toExp $ opFun op a b
    e                               -> e

propQuad :: Consts -> Quad -> Quad
propQuad cs = \case
    Assign v e        -> Assign v $ propExp cs e
    CondJump a op b l -> CondJump (propArg cs a) op (propArg cs b) l
    Exp e             -> Exp $ propExp cs e
    q                 -> q

propExp :: Consts -> Exp -> Exp
propExp cs = \case
    BinInt a op b -> BinInt (propArg cs a) op (propArg cs b)
    Val a         -> Val (propArg cs a)
    Call f as     -> Call f (map (propArg cs) as)
    e             -> e

propArg :: Consts -> Arg -> Arg
propArg (Consts cs) (Var v) = maybe (Var v) ConstI $ cs ^. at v
propArg _ x                 = x

toExp :: Integer -> Exp
toExp = Val . ConstI

opFun :: Integral a => BinOp -> a -> a -> a
opFun = \case
    Plus  -> (+)
    Minus -> (-)
    Times -> (*)
    Div   -> quot
    Mod   -> rem

relFun :: (Eq a, Ord a) => RelOp -> a -> a -> Bool
relFun = \case
    LT -> (<)
    LE -> (<=)
    GT -> (>)
    GE -> (>=)
    EQ -> (==)
    NE -> (/=)
