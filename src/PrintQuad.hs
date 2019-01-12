{-# LANGUAGE LambdaCase #-}
module PrintQuad where

import Quad
import Data.Monoid
import Data.List
import Prelude hiding (EQ, LT, GT)

printFunDef :: TopDef -> String
printFunDef (FunDef _ f vs qs) =
    "define " <> f <> "(" <> pVars vs <> ") {\n" <> pQs qs <> "\n}"

pVars :: [Var] -> String
pVars = intercalate ","

pQs :: [Quad] -> String
pQs = intercalate "\n" . map pQ

pQ :: Quad -> String
pQ = \case
    q@(Mark _) -> printQuad q
    q          -> "    " <> printQuad q

printQuad :: Quad -> String
printQuad = \case
    Assign v e          -> v <> " = " <> printExp e
    Jump l              -> "goto " <> name l
    Mark l              -> name l <> ":"
    CondJump a1 op a2 l -> "if " <> printArg a1 <> " " <> printRelOp op <> " " <> printArg a2 <> " goto " <> name l
    Exp e               -> printExp e

printExp :: Exp -> String
printExp = \case
    BinInt a1 op a2 -> printArg a1 <> " " <> printBinOp op <> " " <> printArg a2
    Load l          -> "load " <> name l
    Call f as       -> f <> "(" <> (intercalate "," $ map printArg as) <> ")"
    Val a           -> printArg a

printArg :: Arg -> String
printArg = \case
    ConstI x   -> show x
    Var v      -> v

printBinOp :: BinOp -> String
printBinOp = \case
    Plus  -> "+"
    Minus -> "-"
    Times -> "*"
    Div   -> "/"
    Mod   -> "%"

printRelOp :: RelOp -> String
printRelOp = \case
    LT -> "<"
    LE -> "<="
    GT -> ">"
    GE -> ">="
    EQ -> "=="
    NE -> "!="
