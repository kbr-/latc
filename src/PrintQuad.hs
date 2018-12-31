{-# LANGUAGE LambdaCase #-}
module PrintQuad where

import Quad
import Data.Monoid
import Data.List
import Prelude hiding (EQ, LT, GT)

printFunDef :: FunDef -> String
printFunDef (FunDef _ f vs qs) =
    "define " <> f <> "(" <> pVars vs <> ") {\n" <> pQs qs <> "\n}"

pVars :: [Var] -> String
pVars = concat . intersperse ","

pQs :: [Quad] -> String
pQs = concat . intersperse "\n" . map pQ

pQ :: Quad -> String
pQ = \case
    q@(Mark _) -> printQuad q
    q          -> "    " <> printQuad q

printQuad :: Quad -> String
printQuad = \case
    Assign v e          -> v <> " = " <> printExp e
    Jump l              -> "goto " <> l
    Mark l              -> l <> ":"
    CondJump a1 op a2 l -> "if " <> printArg a1 <> " " <> printRelOp op <> " " <> printArg a2 <> " goto " <> l
    Exp e               -> printExp e

printExp :: Exp -> String
printExp = \case
    BinInt a1 op a2 -> printArg a1 <> " " <> printBinOp op <> " " <> printArg a2
    AddStr a1 a2    -> printArg a1 <> " ++ " <> printArg a2
    Call f as       -> f <> "(" <> (concat $ intersperse "," $ map printArg as) <> ")"
    Val a           -> printArg a

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
