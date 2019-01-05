{-# LANGUAGE LambdaCase #-}
module Common where

import Quad

indexed :: [a] -> [(Int, a)]
indexed = reverse . fst . foldl (\(xs, i) x -> ((i,x):xs, i+1)) ([], 0)

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
