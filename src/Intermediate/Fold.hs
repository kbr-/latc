{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
module Intermediate.Fold where

import Prelude hiding (LT, GT, EQ)
import Control.Lens
import Control.Exception
import Data.Maybe
import Data.List
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

fold :: Graph Block -> [Defs] -> [Block]
fold g = clean . P.propagate @Consts g

clean :: [Block] -> [Block]
clean = cleanUnreachable . mkGraph . mapMaybe cleanJumps . concat

cleanUnreachable :: Graph Block -> [Block]
cleanUnreachable Graph{..} = map snd $ filter ((reachables !!) . fst) $ Common.indexed vertices
  where
    reachables = fixed step start
    start = True : (map (const False) $ [1..cnt])
    step rs = map (reachable rs) [0..cnt]
    reachable rs i = rs !! i || (any $ \(j, ds) -> elem i ds && rs !! j) ies
    ies = Common.indexed edges
    cnt = length vertices - 1

cleanJumps :: Quad -> Maybe Quad
cleanJumps = \case
    CondJump (ConstI a) op (ConstI b) l -> if relFun op a b then Just $ Jump l else Nothing
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
    Store p a         -> Store (propPtr cs p) (propArg cs a)
    CondJump a op b l -> CondJump (propArg cs a) op (propArg cs b) l
    Exp e             -> Exp $ propExp cs e
    q                 -> q

propExp :: Consts -> Exp -> Exp
propExp cs = \case
    BinInt a op b -> BinInt (propArg cs a) op (propArg cs b)
    Val a         -> Val (propArg cs a)
    Call f as     -> Call f (map (propArg cs) as)
    LoadPtr p     -> LoadPtr $ propPtr cs p
    e             -> e

propPtr :: Consts -> Ptr -> Ptr
propPtr cs (Ptr b i d) = Ptr b (propArg cs i) d

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
