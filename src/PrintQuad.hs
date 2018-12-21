{-# LANGUAGE LambdaCase #-}
module PrintQuad where

import Quad
import Data.Monoid
import Data.List
import Prelude hiding (EQ, LT, GT)

printFunDef :: FunDef -> String
printFunDef (FunDef f vs qs) =
    "define " <> f <> "(" <> (concat $ intersperse "," $ vs) <> ") {\n" <>
        (concat $ intersperse "\n" $ map pQ qs) <> "\n}"

pQ :: Quad -> String
pQ = \case
    q@(Mark _) -> printQuad q
    q          -> "    " <> printQuad q

printQuad :: Quad -> String
printQuad = \case
    BinInt v a1 op a2   -> v <> " := " <> printArg a1 <> " " <> printBinOp op <> " " <> printArg a2
    AddStr v a1 a2      -> v <> " := " <> printArg a1 <> " ++ " <> printArg a2
    Neg v a             -> v <> " := -" <> printArg a
    Move v a            -> v <> " := " <> printArg a
    Jump l              -> "goto " <> l
    Mark l              -> l <> ":"
    CondJump a1 op a2 l -> "if " <> printArg a1 <> " " <> printRelOp op <> " " <> printArg a2 <> " goto " <> l
    Call v f as         -> v <> " := " <> f <> "(" <> (concat $ intersperse "," $ map printArg as) <> ")"
    Ret a               -> "return " <> printArg a
    VRet                -> "return"

printArg :: Arg -> String
printArg = \case
    ConstS str -> str
    ConstI x   -> show x
    Var v      -> v

printBinOp :: BinOp -> String
printBinOp = \case
    Plus  -> "+"
    Minus -> "-"
    Times -> "*"
    Div   -> "/"
    Mod   -> "%"
    Xor   -> "^"

printRelOp :: RelOp -> String
printRelOp = \case
    LT -> "<"
    LE -> "<="
    GT -> ">"
    GE -> ">="
    EQ -> "=="
    NE -> "!="
