module Annotated where

import qualified Data.Map as M

type Ident = String

type Program = [TopDef]

data TopDef = FunDef
    { funType :: FunType
    , funIdent :: Ident
    , args :: M.Map VarId ArgInfo
    , locals :: M.Map VarId VarInfo -- all local variables, including arguments
    , body :: Block
    }

type VarId = Int

data VarInfo = VarInfo Ident Type

data ArgInfo = ArgInfo Int VarInfo
--                     ^ the argument's position

type Block = [Stmt]

data Stmt
    = Empty
    | BStmt Block
    | Decl Type [Item]
    | Ass LVal Expr
    | Incr VarId
    | Decr VarId
    | Ret Expr
    | VRet
    | Cond Expr Stmt
    | CondElse Expr Stmt Stmt
    | While Expr Stmt
    | ForEach VarId VarId Stmt
    | SExp Expr

data Item = NoInit VarId | Init VarId Expr

data Type = Int | Str | Bool | Void | Arr Type | Struct Ident [(Ident, Type)]
    deriving Show

instance Eq Type where
 Int == Int = True
 Str == Str = True
 Bool == Bool = True
 Void == Void = True
 Arr t == Arr t' | t == t' = True
 Struct i _ == Struct i' _ | i == i' = True
 _ == _ = False

data FunType = FunType Type [Type]

data Expr
    = EVar LVal
    | ELitInt Integer
    | ELitTrue
    | ELitFalse
    | EApp Ident [Expr]
    | ENewArr Expr
    | ENewStruct Integer
    | ENull
    | EString String
    | Neg Expr
    | Not Expr
    | EMul Expr MulOp Expr
    | EAddInt Expr AddOp Expr
    | EAddString Expr Expr
    | ERel Expr RelOp Expr
    | EAnd Expr Expr
    | EOr Expr Expr
    deriving Eq

data LVal
    = Var VarId
    | ArrElem VarId Expr
    | Attr VarId Attr
    deriving Eq

data Attr
    = ALeaf Ident
    | AArr Ident Expr
    | AStruct Ident Attr
    deriving Eq

data AddOp = Plus | Minus
  deriving (Eq, Ord, Show, Read)

data MulOp = Times | Div | Mod
  deriving (Eq, Ord, Show, Read)

data RelOp = LTH | LE | GTH | GE | EQU | NE
  deriving (Eq, Ord, Show, Read)

