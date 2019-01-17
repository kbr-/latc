module Quad where

data Program = Program Consts [TopDef]

type Consts = [(String, Label)]

data TopDef = FunDef Bool String [Var] [Quad]

data Quad
    = Assign Var Exp
    | Store Ptr Arg
    | Jump Label
    | Mark Label
    | CondJump Arg RelOp Arg Label
    | Exp Exp
    deriving Eq

data Exp
    = BinInt Arg BinOp Arg
    | Load Label
    | Val Arg
    | Call Fun [Arg]
    | LoadPtr Ptr
    deriving (Eq, Ord)

type Var = String

data Ptr = Ptr Var Arg
    deriving (Eq, Ord)

data Arg
    = ConstI Integer
    | Var Var
    deriving (Eq, Ord)

data BinOp
    = Plus
    | Minus
    | Times
    | Div
    | Mod
    deriving (Eq, Ord)

newtype Label = Label { name :: String }
    deriving (Eq, Ord)

data RelOp
    = LT
    | LE
    | GT
    | GE
    | EQ
    | NE
    deriving Eq

type Fun = String

retVar :: String
retVar = "r"

lRet :: Fun -> Label
lRet f = Label $ "L" ++ f ++ "_ret"
