{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Common where

import Data.List
import Data.Function

import Quad

indexed :: [a] -> [(Int, a)]
indexed = zip [0..]

fixed :: Eq a => (a -> a) -> a -> a
fixed f = head . dropWhile (\x -> x /= f x) . iterate f

vars :: Exp -> [Var]
vars = \case
    BinInt a1 _ a2 -> vars' [a1, a2]
    Load _         -> []
    Val a          -> vars' [a]
    Call _ as      -> vars' as

vars' :: [Arg] -> [Var]
vars' = concatMap $ \case
    Var v -> [v]
    _     -> []

hasEffects :: Exp -> Bool
hasEffects = \case
    Call _ _ -> True
    _        -> False

sortWithM :: (Applicative m, Ord b) => (a -> m b) -> [a] -> m [a]
sortWithM f l = (map fst . sortBy (compare `on` snd)) <$> traverse (\x -> (x, ) <$> f x) l
