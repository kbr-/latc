{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Semantic.Expr where

import qualified Annotated as AT
import qualified AbsLatte as T
import qualified Data.Map as M
import Data.Functor.Identity
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader

type ZE = ExceptT ErrT (ReaderT Symbols Identity)

data Symbols = Symbols
    { vars :: M.Map AT.Ident (AT.VarId, AT.Type)
    , funs :: M.Map AT.Ident AT.FunType
    }

type ErrT = String

getVar :: AT.Ident -> ZE (AT.VarId, AT.Type)
getVar str =
    reader (M.lookup str . vars) >>= \case
        Just x  -> pure x
        Nothing -> throwError $ undeclaredVariable str

getFunType :: AT.Ident -> ZE AT.FunType
getFunType str =
    reader (M.lookup str . funs) >>= \case
        Just x  -> pure x
        Nothing -> throwError $ unknownFun str

expr :: T.Expr -> ZE (AT.Expr, AT.Type)

expr (T.EVar (T.Ident str)) = do
    (varId, t) <- getVar str
    pure (AT.EVar (AT.LVal varId), t)

expr (T.ELitInt x) =
    pure (AT.ELitInt x, AT.Int)

expr T.ELitTrue =
    pure (AT.ELitTrue, AT.Bool)

expr T.ELitFalse =
    pure (AT.ELitFalse, AT.Bool)

expr (T.EApp (T.Ident str) es) = do
    aes <- mapM expr es
    AT.FunType retType argTypes <- getFunType str
    let ts = map snd aes
    unless (argTypes == ts) $ throwError $ argTypeMismatch argTypes ts
    pure (AT.EApp str (map fst aes), retType)

expr (T.EString str) =
    pure (AT.EString str, AT.Str)

expr (T.Neg e) = do
    (ae, t) <- expr e
    unless (t == AT.Int) $ throwError $ cannotNeg t
    pure (AT.Neg ae, AT.Int)

expr (T.Not e) = do
    (ae, t) <- expr e
    unless (t == AT.Bool) $ throwError $ cannotNot t
    pure (AT.Not ae, AT.Bool)

expr (T.EMul e1 (annMulOp -> op) e2) = do
    (ae1, t1) <- expr e1
    (ae2, t2) <- expr e2
    case (t1, t2) of
        (AT.Int, AT.Int) -> pure (AT.EMul ae1 op ae2, AT.Int)
        (_, _)           -> throwError $ cannotMul t1 t2

expr (T.EAdd e1 (annAddOp -> op) e2) = do
    (ae1, t1) <- expr e1
    (ae2, t2) <- expr e2
    case (t1, t2, op) of
        (AT.Int, AT.Int, op)      -> pure (AT.EAddInt ae1 op ae2, AT.Int)
        (AT.Str, AT.Str, AT.Plus) -> pure (AT.EAddString ae1 ae2, AT.Str)
        (_, _, _)                 -> throwError $ cannotAdd t1 t2

expr (T.ERel e1 (annRelOp -> op) e2) = do
    (ae1, t1) <- expr e1
    (ae2, t2) <- expr e2
    case (t1, t2) of
        (AT.Int, AT.Int) -> pure (AT.ERel ae1 op ae2, AT.Bool)
        (_, _)           -> throwError $ cannotCompare t1 t2

expr (T.EAnd e1 e2) =
    boolOp AT.EAnd e1 e2

expr (T.EOr e1 e2) =
    boolOp AT.EOr e1 e2

boolOp cons e1 e2 = do
    (ae1, t1) <- expr e1
    (ae2, t2) <- expr e2
    case (t1, t2) of
        (AT.Bool, AT.Bool) -> pure (cons ae1 ae2, AT.Bool)
        (_, _) -> throwError $ cannotBoolOp t1 t2

undeclaredVariable :: AT.Ident -> ErrT
undeclaredVariable = undefined

unknownFun :: AT.Ident -> ErrT
unknownFun = undefined

annAddOp :: T.AddOp -> AT.AddOp
annAddOp = undefined

annMulOp :: T.MulOp -> AT.MulOp
annMulOp = undefined

annRelOp :: T.RelOp -> AT.RelOp
annRelOp = undefined

argTypeMismatch :: [AT.Type] -> [AT.Type] -> ErrT
argTypeMismatch = undefined

typeMismatch :: AT.Type -> AT.Type -> ErrT
typeMismatch = undefined

cannotMul :: AT.Type -> AT.Type -> ErrT
cannotMul = undefined

cannotAdd :: AT.Type -> AT.Type -> ErrT
cannotAdd = undefined

cannotNeg :: AT.Type -> ErrT
cannotNeg = undefined

cannotNot :: AT.Type -> ErrT
cannotNot = undefined

cannotCompare :: AT.Type -> AT.Type -> ErrT
cannotCompare = undefined

cannotBoolOp :: AT.Type -> AT.Type -> ErrT
cannotBoolOp = undefined
