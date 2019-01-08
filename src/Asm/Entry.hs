{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Asm.Entry where

import Control.Monad.Writer.Class
import Data.Monoid

import qualified Quad as Q

newtype Reg = Reg { regName :: String }
    deriving (Eq, Ord, Show)

type Offset = Integer

newtype Memloc = Memloc { offset :: Integer }
    deriving (Eq, Ord, Show)

data Arg
    = AReg Reg
    | AMem Memloc
    | AConst Integer
    | ALab Q.Label

type Entry = String

emit :: MonadWriter [Entry] m => Entry -> m ()
emit = tell . pure

emitWithComment :: MonadWriter [Entry] m => Entry -> String -> m ()
emitWithComment e c = tell . pure $ e <> " # " <> c

emitComment :: MonadWriter [Entry] m => String -> m ()
emitComment s = tell . pure $ "# " <> s

label :: Q.Label -> Entry
label = (<> ":") . Q.name

jmp :: Q.Label -> Entry
jmp = ("jmp " <>) . Q.name

jop :: Q.RelOp -> Q.Label -> Entry
jop op l = (case op of
    Q.LT -> "jl"
    Q.LE -> "jle"
    Q.GT -> "jg"
    Q.GE -> "jge"
    Q.EQ -> "je"
    Q.NE -> "jne") <> " " <> Q.name l

pushl :: Arg -> Entry
pushl = unOp "pushl"

popl :: Arg -> Entry
popl = unOp "popl"

idivl :: Arg -> Entry
idivl = unOp "idivl"

movl :: Arg -> Arg -> Entry
movl = binOp "movl"

leal :: Arg -> Arg -> Entry
leal = binOp "leal"

addl :: Arg -> Arg -> Entry
addl = oper Q.Plus

subl :: Arg -> Arg -> Entry
subl = oper Q.Minus

cmpl :: Arg -> Arg -> Entry
cmpl = binOp "cmpl"

oper :: Q.BinOp -> Arg -> Arg -> Entry
oper op = binOp (case op of
    Q.Plus  -> "addl"
    Q.Minus -> "subl"
    Q.Times -> "imull"
    Q.Xor   -> "xorl")

calll :: Q.Fun -> Entry
calll f = "calll " <> f

cdq :: Entry
cdq = "cdq"

binOp :: String -> Arg -> Arg -> Entry
binOp op a1 a2 = op <> " " <> printArg a1 <> ", " <> printArg a2

unOp :: String -> Arg -> Entry
unOp op a = op <> " " <> printArg a

printArg :: Arg -> String
printArg = \case
    AReg (Reg s)      -> "%" <> s
    AMem (Memloc off) -> show off <> "(%ebp)"
    AConst i          -> "$" <> show i
    ALab l            -> Q.name l

retl :: Entry
retl = "retl"

asciz :: String -> Entry
asciz s = ".asciz " <> s

globl :: String -> Entry
globl s = ".globl " <> s

mem :: Memloc -> Arg
mem = AMem

reg :: Reg -> Arg
reg = AReg

con :: Integer -> Arg
con = AConst

lab :: Q.Label -> Arg
lab = ALab

ebp :: Reg
ebp = Reg "ebp"

esp :: Reg
esp = Reg "esp"

eax :: Reg
eax = Reg "eax"

ecx :: Reg
ecx = Reg "ecx"

edx :: Reg
edx = Reg "edx"

ebx :: Reg
ebx = Reg "ebx"

esi :: Reg
esi = Reg "esi"

edi :: Reg
edi = Reg "edi"
