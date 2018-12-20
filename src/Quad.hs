module Quad where

data FunDef = FunDef String [Var] [Quad]

data Quad
    = BinInt Var Arg BinOp Arg
    | AddStr Var Arg Arg
    | Neg Var Arg
    | Move Var Arg
    | Jump Label
    | Mark Label
    | CondJump Arg RelOp Arg Label
    | Call Var Fun [Arg]
    | Ret Arg
    | VRet

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
