module Quad where

data Program = Program Consts [TopDef]

type Consts = [(String, Label)]

data TopDef = FunDef Bool String [Var] [Quad]

data Quad
    = Assign Var Exp
    | Jump Label
    | Mark Label
    | CondJump Arg RelOp Arg Label
    | Exp Exp

data Exp
    = BinInt Arg BinOp Arg
    | Load Label
    | Val Arg
    | Call Fun [Arg]

type Var = String

data Arg
    = ConstI Integer
    | Var Var
    deriving Eq

data BinOp
    = Plus
    | Minus
    | Times
    | Div
    | Mod
    | Xor
    deriving Eq

newtype Label = Label { name :: String }
    deriving (Eq, Ord)

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

lRet :: Fun -> Label
lRet f = Label $ "L" ++ f ++ "_ret"
