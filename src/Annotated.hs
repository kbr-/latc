module Annotated where

import qualified Data.Map as M

type Ident = String

type Program = [TopDef]

data TopDef = FunDef
    { funType :: FunType
    , funIdent :: Ident
    , args :: M.Map VarId ArgInfo
    , locals :: M.Map VarId VarInfo
    , body :: Block
    }

type VarId = Int

data VarInfo = VarInfo Ident Type

data ArgInfo = ArgInfo Int VarInfo

type Block = [Stmt]

data Stmt
    = Empty
    | BStmt Block
    | Decl Type [Item]
    | Ass LVal Expr
    | Incr LVal
    | Decr LVal
    | Ret Expr
    | VRet
    | Cond Expr Stmt
    | CondElse Expr Stmt Stmt
    | While Expr Stmt
    | SExp Expr

data Item = NoInit LVal | Init LVal Expr

data Type = Int | Str | Bool | Void
    deriving (Eq, Show)

data FunType = FunType Type [Type]

data Expr
    = EVar LVal
    | ELitInt Integer
    | ELitTrue
    | ELitFalse
    | EApp Ident [Expr]
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

data LVal = LVal VarId
    deriving Eq

data AddOp = Plus | Minus
  deriving (Eq, Ord, Show, Read)

data MulOp = Times | Div | Mod
  deriving (Eq, Ord, Show, Read)

data RelOp = LTH | LE | GTH | GE | EQU | NE
  deriving (Eq, Ord, Show, Read)

