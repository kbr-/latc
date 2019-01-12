{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Intermediate.Generate where

import qualified Annotated as T
import Quad
import Data.Functor.Identity
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Control.Lens
import Control.Monad.RWS.Strict
import Control.Monad.State.Strict
import Control.Exception.Base
import GHC.Exts (sortWith)
import qualified Data.Map as M
import Prelude hiding (EQ, LT, GT)

data ProgSt = ProgSt
    { _consts     :: M.Map String Label
    , _codeLabels :: Int
    , _strLabels  :: Int
    }

data GenSt = GenSt
    { _progSt :: ProgSt
    , _temps :: Int
    }

data GenEnv = GenEnv
    { locals   :: M.Map T.VarId T.VarInfo
    , funIdent :: Fun
    }

type Gen = RWS GenEnv [Quad] GenSt

makeLenses ''ProgSt
makeLenses ''GenSt

getVar :: T.LVal -> Gen String
getVar (T.LVal x) = do
    var <- reader $ M.lookup x . locals
    assert (isJust var) $ case var of
        (Just (T.VarInfo ident _)) -> pure $ ident <> show x

newTemp :: Gen String
newTemp = ("t" <>) . show <$> (temps <<+= 1)

newLabel :: Gen Label
newLabel = zoom progSt $ Label . (".L" <>) . show <$> (codeLabels <<+= 1)

getString :: String -> Gen Label
getString s = zoom progSt $ do
    l <- use $ consts . at s
    case l of
        Just l  -> pure l
        Nothing -> do
            l <- Label . (".L.str" <>) . show <$> (strLabels <<+= 1)
            consts . at s ?= l
            pure l

expTemp :: Exp -> Gen Var
expTemp e = do
    t <- newTemp
    emit $ Assign t e
    pure t

emit :: Quad -> Gen ()
emit q = tell [q]

program :: T.Program -> Program
program p = Program (M.toList _consts) ds
  where
    (ds, ProgSt{..}) = runState (traverse topDef p) $ ProgSt M.empty 0 0

topDef :: T.TopDef -> State ProgSt TopDef
topDef T.FunDef{..} = do
    s <- GenSt <$> get <*> pure 0
    let (s', qs) = execRWS (traverse_ stmt body) GenEnv{..} s
    put $ _progSt s'
    pure $ FunDef rets funIdent args' (qs <> [Mark $ lRet funIdent])
  where
    rets = case funType of
        (T.FunType T.Void _) -> False
        _                    -> True
    args' = map (\(x, T.ArgInfo _ (T.VarInfo ident _)) -> ident <> show x)
              . sortWith (\(_, T.ArgInfo i _) -> i) . M.toList $ args

stmt :: T.Stmt -> Gen ()

stmt T.Empty = pure ()

stmt (T.BStmt xs) = traverse_ stmt xs

stmt (T.Decl typ xs) = for_ xs $ \case
    T.NoInit v -> Assign <$> getVar v <*> def >>= emit
    T.Init v e -> Assign <$> getVar v <*> expr e >>= emit
  where
    def = case typ of
        T.Str -> Load <$> getString "\"\""
        _     -> pure $ Val $ ConstI 0

stmt (T.Ass v e) = Assign <$> getVar v <*> expr e >>= emit

stmt (T.Incr v) = getVar v >>= \v -> emit $ Assign v $ BinInt (Var v) Plus (ConstI 1)

stmt (T.Decr v) = getVar v >>= \v -> emit $ Assign v $ BinInt (Var v) Minus (ConstI 1)

stmt (T.Ret e) = do
    emit =<< Assign retVar <$> expr e
    emit . Jump . lRet =<< reader funIdent

stmt T.VRet = emit . Jump . lRet =<< reader funIdent

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

stmt (T.SExp e) = Exp <$> expr e >>= emit

jumpIfFalse :: T.Expr -> Label -> Gen ()
jumpIfFalse e l = case e of
    T.ELitTrue -> pure ()
    T.ELitFalse -> emit $ Jump l
    T.Not e -> jumpIfTrue e l
    T.ERel e1 (relOp -> op) e2 -> do
        t1 <- argExpr e1
        t2 <- argExpr e2
        emit $ CondJump t1 (negRelOp op) t2 l
    T.EAnd e1 e2 -> do
        jumpIfFalse e1 l
        jumpIfFalse e2 l
    T.EOr e1 e2 -> do
        lEnd <- newLabel
        jumpIfTrue e1 lEnd
        jumpIfFalse e2 l
        emit $ Mark lEnd
    e -> argExpr e >>= \t -> emit $ CondJump t EQ (ConstI 0) l

jumpIfTrue :: T.Expr -> Label -> Gen ()
jumpIfTrue e l = case e of
    T.ELitTrue -> emit $ Jump l
    T.ELitFalse -> pure ()
    T.Not e -> jumpIfFalse e l
    T.ERel e1 (relOp -> op) e2 -> do
        t1 <- argExpr e1
        t2 <- argExpr e2
        emit $ CondJump t1 op t2 l
    T.EAnd e1 e2 -> do
        lEnd <- newLabel
        jumpIfFalse e1 lEnd
        jumpIfTrue e2 l
        emit $ Mark lEnd
    T.EOr e1 e2 -> do
        jumpIfTrue e1 l
        jumpIfTrue e2 l
    e -> argExpr e >>= \t -> emit $ CondJump t NE (ConstI 0) l

argExpr :: T.Expr -> Gen Arg
argExpr e = expr e >>= \case
    Val a -> pure a
    e     -> Var <$> expTemp e

expr :: T.Expr -> Gen Exp

expr (T.EVar v) = Val . Var <$> getVar v

expr (T.ELitInt x) = pure . Val $ ConstI x

expr T.ELitTrue = pure . Val $ ConstI 1

expr T.ELitFalse = pure . Val $ ConstI 0

expr (T.EApp f es) = Call f <$> traverse argExpr es

expr (T.EString x) = Load <$> getString x

expr (T.Neg e) = BinInt (ConstI 0) Minus <$> argExpr e

expr (T.Not e) = BinInt (ConstI 1) Minus <$> argExpr e

expr (T.EMul e1 (mulOp -> op) e2) = BinInt <$> argExpr e1 <*> pure op <*> argExpr e2 -- TODO: opt order

expr (T.EAddInt e1 (addOp -> op) e2) = BinInt <$> argExpr e1 <*> pure op <*> argExpr e2

expr (T.EAddString e1 e2) = Call "_concat" <$> traverse argExpr [e1, e2]

expr (T.ERel e1 (relOp -> op) e2) = do
    t1 <- argExpr e1
    t2 <- argExpr e2
    r <- newTemp
    lTrue <- newLabel
    lEnd <- newLabel
    traverse emit
        [ CondJump t1 op t2 lTrue
        , Assign r (Val $ ConstI 0)
        , Jump lEnd
        , Mark lTrue
        , Assign r (Val $ ConstI 1)
        , Mark lEnd
        ]
    pure $ Val $ Var r

expr (T.EAnd e1 e2) = do
    t1 <- expr e1
    r <- newTemp
    lEnd <- newLabel
    traverse emit
        [ Assign r t1
        , CondJump (Var r) EQ (ConstI 0) lEnd
        ]
    t2 <- expr e2
    traverse emit
        [ Assign r t2
        , Mark lEnd
        ]
    pure $ Val $ Var r

expr (T.EOr e1 e2) = do
    t1 <- expr e1
    r <- newTemp
    lEnd <- newLabel
    traverse emit
        [ Assign r t1
        , CondJump (Var r) NE (ConstI 0) lEnd
        ]
    t2 <- expr e2
    traverse emit
        [ Assign r t2
        , Mark lEnd
        ]
    pure $ Val $ Var r

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
