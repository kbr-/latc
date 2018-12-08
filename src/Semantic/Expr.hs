{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
module Semantic.Expr where

import qualified Annotated as AT
import qualified AbsLatte as T
import qualified Data.Map as M
import Semantic.Common
import Data.Monoid
import Data.Functor.Identity
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader

type ZE = ExceptT Err (ReaderT Symbols Identity)

data Symbols = Symbols
    { vars :: M.Map AT.Ident (AT.VarId, AT.Type)
    , funs :: M.Map AT.Ident AT.FunType
    }

runZE :: Symbols -> ZE (AT.Expr, AT.Type) -> Either Err (AT.Expr, AT.Type)
runZE sym = runIdentity . flip runReaderT sym . runExceptT

expr :: T.Expr Pos -> ZE (AT.Expr, AT.Type)

expr (T.EVar pos (T.Ident str)) = do
    (varId, t) <- reader (M.lookup str . vars) >>= \case
        Just x  -> pure x
        Nothing -> errorWithPos pos $ undeclaredVariable str
    pure (AT.EVar (AT.LVal varId), t)

expr (T.ELitInt _ x) =
    pure (AT.ELitInt x, AT.Int)

expr (T.ELitTrue _) =
    pure (AT.ELitTrue, AT.Bool)

expr (T.ELitFalse _) =
    pure (AT.ELitFalse, AT.Bool)

expr (T.EApp pos (T.Ident str) es) = do
    aes <- mapM expr es
    AT.FunType retType argTypes <- reader (M.lookup str . funs) >>= \case
        Just x  -> pure x
        Nothing -> errorWithPos pos $ unknownFun str
    let ts = map snd aes
    unless (argTypes == ts) $ errorWithPos pos $ argTypeMismatch argTypes ts
    pure (AT.EApp str (map fst aes), retType)

expr (T.EString _ str) =
    pure (AT.EString str, AT.Str)

expr (T.Neg pos e) = do
    (ae, t) <- expr e
    unless (t == AT.Int) $ errorWithPos pos $ cannotNeg t
    pure (AT.Neg ae, AT.Int)

expr (T.Not pos e) = do
    (ae, t) <- expr e
    unless (t == AT.Bool) $ errorWithPos pos $ cannotNot t
    pure (AT.Not ae, AT.Bool)

expr (T.EMul pos e1 (annMulOp -> op) e2) = do
    (ae1, t1) <- expr e1
    (ae2, t2) <- expr e2
    case (t1, t2) of
        (AT.Int, AT.Int) -> pure (AT.EMul ae1 op ae2, AT.Int)
        (_, _)           -> errorWithPos pos $ cannotMul t1 t2

expr (T.EAdd pos e1 (annAddOp -> op) e2) = do
    (ae1, t1) <- expr e1
    (ae2, t2) <- expr e2
    case (t1, t2, op) of
        (AT.Int, AT.Int, op)      -> pure (AT.EAddInt ae1 op ae2, AT.Int)
        (AT.Str, AT.Str, AT.Plus) -> pure (AT.EAddString ae1 ae2, AT.Str)
        (_, _, _)                 -> errorWithPos pos $ cannotAdd t1 t2

expr (T.ERel pos e1 (annRelOp -> op) e2) = do
    (ae1, t1) <- expr e1
    (ae2, t2) <- expr e2
    case (t1, t2) of
        (AT.Int, AT.Int)                          -> pure (AT.ERel ae1 op ae2, AT.Bool)
        _ | t1 == t2 && op `elem` [AT.EQU, AT.NE] -> pure (AT.ERel ae1 op ae2, AT.Bool)
        _ | t1 == t2                              -> errorWithPos pos $ cannotCompareOrd t1
        _                                         -> errorWithPos pos $ cannotCompare t1 t2

expr (T.EAnd pos e1 e2) =
    boolOp pos AT.EAnd e1 e2

expr (T.EOr pos e1 e2) =
    boolOp pos AT.EOr e1 e2

boolOp pos cons e1 e2 = do
    (ae1, t1) <- expr e1
    (ae2, t2) <- expr e2
    case (t1, t2) of
        (AT.Bool, AT.Bool) -> pure (cons ae1 ae2, AT.Bool)
        (_, _)             -> errorWithPos pos $ cannotBoolOp t1 t2

annAddOp :: T.AddOp a -> AT.AddOp
annAddOp (T.Plus _)  = AT.Plus
annAddOp (T.Minus _) = AT.Minus

annMulOp :: T.MulOp a -> AT.MulOp
annMulOp (T.Times _) = AT.Times
annMulOp (T.Div _)   = AT.Div
annMulOp (T.Mod _)   = AT.Mod

annRelOp :: T.RelOp a -> AT.RelOp
annRelOp (T.LTH _) = AT.LTH
annRelOp (T.LE _)  = AT.LE
annRelOp (T.GTH _) = AT.GTH
annRelOp (T.GE _)  = AT.GE
annRelOp (T.EQU _) = AT.EQU
annRelOp (T.NE _)  = AT.NE

unknownFun :: AT.Ident -> Err
unknownFun ident =
    "Unknown function: " <> ident

argTypeMismatch :: [AT.Type] -> [AT.Type] -> Err
argTypeMismatch args given =
    "Type of a function argument doesn't match the type of given expression.\n\
    \Function argument types are: " <> show args <> "\n\
    \Given types are: " <> show given

cannotMul :: AT.Type -> AT.Type -> Err
cannotMul t1 t2 =
    "Cannot multiply expressions of type " <> show t1 <> " and " <> show t2

cannotAdd :: AT.Type -> AT.Type -> Err
cannotAdd t1 t2 =
    "Cannot add expressions of type " <> show t1 <> " and " <> show t2

cannotNeg :: AT.Type -> Err
cannotNeg t =
    "Cannot negate expression of type " <> show t

cannotNot :: AT.Type -> Err
cannotNot t =
    "Cannot negate expression of type " <> show t

cannotCompare :: AT.Type -> AT.Type -> Err
cannotCompare t1 t2 =
    "Cannot compare expressions of type " <> show t1 <> " and " <> show t2

cannotCompareOrd :: AT.Type -> Err
cannotCompareOrd t1 =
    "Cannot compare expressions of type " <> show t1 <> " for ordering"

cannotBoolOp :: AT.Type -> AT.Type -> Err
cannotBoolOp t1 t2 =
    "Cannot perform logical operation on expressions of type " <> show t1 <> " and " <> show t2
