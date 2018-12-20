{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Intermediate.Generate where

import qualified Annotated as T
import Quad
import Data.Functor.Identity
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Exception.Base
import GHC.Exts (sortWith)
import qualified Data.Map as M
import Prelude hiding (EQ, LT, GT)

data GenSt = GenSt
    { temps  :: Int
    , labels :: Int
    }

type GenQ = WriterT [Quad] (StateT GenSt (Reader (M.Map T.VarId T.VarInfo)))

getVar :: T.LVal -> GenQ String
getVar (T.LVal x) = do
    var <- reader (M.lookup x)
    assert (isJust var) $ case var of
        (Just (T.VarInfo ident _)) -> pure $ ident <> show x

newTemp :: GenQ String
newTemp = temps <$> get >>= \t -> modify (\s -> s { temps = t + 1 }) *> pure ("t" <> show t)

newLabel :: GenQ String
newLabel = labels <$> get >>= \l -> modify (\s -> s { labels = l + 1 }) *> pure ("l" <> show l)

emit :: Quad -> GenQ ()
emit q = tell [q]

generate :: T.TopDef -> FunDef
generate T.FunDef {..} = FunDef funIdent args' qs
  where args' = map (\(x, T.ArgInfo _ (T.VarInfo ident _)) -> ident <> show x)
              . sortWith (\(_, T.ArgInfo i _) -> i) . M.toList $ args
        qs = flip runReader locals . flip evalStateT (GenSt 0 0) . execWriterT $ traverse_ stmt body

stmt :: T.Stmt -> GenQ ()

stmt T.Empty = pure ()

stmt (T.BStmt xs) = traverse_ stmt xs

stmt (T.Decl typ xs) = for_ xs $ \case
    T.NoInit v -> Move <$> getVar v <*> pure def >>= emit
    T.Init v e -> Move <$> getVar v <*> expr e >>= emit
  where
    def = case typ of
        T.Str -> ConstS "\"\""
        _     -> ConstI 0

stmt (T.Ass v e) = Move <$> getVar v <*> expr e >>= emit

stmt (T.Incr v) = getVar v >>= \v -> emit $ BinInt v (Var v) Plus (ConstI 1)

stmt (T.Decr v) = getVar v >>= \v -> emit $ BinInt v (Var v) Minus (ConstI 1)

stmt (T.Ret e) = Ret <$> expr e >>= emit

stmt T.VRet = emit VRet

stmt (T.Cond e s) = case e of
    T.ELitTrue -> stmt s
    T.ELitFalse -> pure ()
    e -> do
        lEnd <- newLabel
        jumpIfFalse e lEnd
        stmt s
        emit $ Mark lEnd

stmt (T.CondElse e s1 s2) = case e of
    T.ELitTrue -> stmt s1
    T.ELitFalse -> stmt s2
    e -> do
        lElse <- newLabel
        lEnd <- newLabel
        jumpIfFalse e lElse
        stmt s1
        emit $ Jump lEnd
        emit $ Mark lElse
        stmt s2
        emit $ Mark lEnd

stmt (T.While e s) = do
    lCond <- newLabel
    lBody <- newLabel
    emit $ Jump lCond
    emit $ Mark lBody
    stmt s
    emit $ Mark lCond
    jumpIfTrue e lBody

stmt (T.SExp e) = expr e *> pure ()

jumpIfFalse :: T.Expr -> Label -> GenQ ()
jumpIfFalse e l = case e of
    T.ELitTrue -> pure ()
    T.ELitFalse -> emit $ Jump l
    T.Not e -> jumpIfTrue e l
    T.ERel e1 (relOp -> op) e2 -> do
        t1 <- expr e1
        t2 <- expr e2
        emit $ CondJump t1 (negRelOp op) t2 l
    T.EAnd e1 e2 -> do
        jumpIfFalse e1 l
        jumpIfFalse e2 l
    T.EOr e1 e2 -> do
        lEnd <- newLabel
        jumpIfTrue e1 lEnd
        jumpIfFalse e2 l
        emit $ Mark lEnd
    e -> expr e >>= \t -> emit $ CondJump t EQ (ConstI 0) l

jumpIfTrue :: T.Expr -> Label -> GenQ ()
jumpIfTrue e l = case e of
    T.ELitTrue -> emit $ Jump l
    T.ELitFalse -> pure ()
    T.Not e -> jumpIfFalse e l
    T.ERel e1 (relOp -> op) e2 -> do
        t1 <- expr e1
        t2 <- expr e2
        emit $ CondJump t1 op t2 l
    T.EAnd e1 e2 -> do
        lEnd <- newLabel
        jumpIfFalse e1 lEnd
        jumpIfTrue e2 l
        emit $ Mark lEnd
    T.EOr e1 e2 -> do
        jumpIfTrue e1 l
        jumpIfTrue e2 l
    e -> expr e >>= \t -> emit $ CondJump t NE (ConstI 0) l

expr :: T.Expr -> GenQ Arg

expr (T.EVar v) = Var <$> getVar v

expr (T.ELitInt x) = pure $ ConstI x

expr T.ELitTrue = pure $ ConstI 1

expr T.ELitFalse = pure $ ConstI 0

expr (T.EApp f es) = do
    ts <- traverse expr es
    r <- newTemp
    emit $ Call r f ts
    pure $ Var r

expr (T.EString x) = pure $ ConstS x

expr (T.Neg e) = do
    t <- expr e
    r <- newTemp
    emit $ Neg r t
    pure $ Var r

expr (T.Not e) = do
    t <- expr e
    r <- newTemp
    emit $ BinInt r t Xor (ConstI 1)
    pure $ Var r

expr (T.EMul e1 (mulOp -> op) e2) = do
    t1 <- expr e1
    t2 <- expr e2 -- TODO: optimize order
    r <- newTemp
    emit $ BinInt r t1 op t2
    pure $ Var r

expr (T.EAddInt e1 (addOp -> op) e2) = do
    t1 <- expr e1
    t2 <- expr e2
    r <- newTemp
    emit $ BinInt r t1 op t2
    pure $ Var r

expr (T.EAddString e1 e2) = do
    t1 <- expr e1
    t2 <- expr e2
    r <- newTemp
    emit $ AddStr r t1 t2
    pure $ Var r

expr (T.ERel e1 (relOp -> op) e2) = do
    t1 <- expr e1
    t2 <- expr e2
    r <- newTemp
    lTrue <- newLabel
    lEnd <- newLabel
    traverse emit 
        [ CondJump t1 op t2 lTrue
        , Move r (ConstI 0)
        , Jump lEnd
        , Mark lTrue
        , Move r (ConstI 1)
        , Mark lEnd
        ]
    pure $ Var r

expr (T.EAnd e1 e2) = do
    t1 <- expr e1
    r <- newTemp
    lEnd <- newLabel
    traverse emit
        [ Move r t1
        , CondJump (Var r) EQ (ConstI 0) lEnd
        ]
    t2 <- expr e2
    traverse emit
        [ Move r t2
        , Mark lEnd
        ]
    pure $ Var r

expr (T.EOr e1 e2) = do
    t1 <- expr e1
    r <- newTemp
    lEnd <- newLabel
    traverse emit
        [ Move r t1
        , CondJump (Var r) NE (ConstI 0) lEnd
        ]
    t2 <- expr e2
    traverse emit
        [ Move r t2
        , Mark lEnd
        ]
    pure $ Var r

mulOp :: T.MulOp -> BinOp
mulOp = \case
    T.Times -> Times
    T.Div   -> Div
    T.Mod   -> Mod

addOp :: T.AddOp -> BinOp
addOp = \case
    T.Plus  -> Plus
    T.Minus -> Minus

relOp :: T.RelOp -> RelOp
relOp = \case
    T.LTH -> LT
    T.LE  -> LE
    T.GTH -> GT
    T.GE  -> GE
    T.EQU -> EQ
    T.NE  -> NE

negRelOp :: RelOp -> RelOp
negRelOp = \case
    LT -> GE
    LE -> GT
    GT -> LE
    GE -> LT
    EQ -> NE
    NE -> EQ
