module Quad where

data FunDef = FunDef String [Var] [Quad]

data Quad
    = Assign Var Exp
    | Jump Label
    | Mark Label
    | CondJump Arg RelOp Arg Label
    | Exp Exp

data Exp
    = BinInt Arg BinOp Arg
    | AddStr Arg Arg
    | Val Arg
    | Call Fun [Arg]

type Var = String

data Arg
    = ConstS String
    | ConstI Integer
    | Var Var

data BinOp
    = Plus
    | Minus
    | Times
    | Div
    | Mod
    | Xor

type Label = String

data RelOp
    = LT
    | LE
    | GT
    | GE
    | EQ
    | NE

type Fun = String

retVar :: String
retVar = "r"

lRet :: Label
lRet = "Lret"
