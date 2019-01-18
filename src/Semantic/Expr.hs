{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Semantic.Expr where

import qualified Annotated as AT
import qualified AbsLatte as T
import qualified Data.Map as M
import Semantic.Common
import Semantic.ErrorT
import Data.Monoid
import Data.Functor.Identity
import Control.Arrow
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader

class HasTypes m => HasSyms m where
    getVar    :: AT.Ident -> m (Maybe (AT.VarId, AT.Type))
    getFun    :: AT.Ident -> m (Maybe AT.FunType)

findMemTyp :: (MonadReport Err m, HasSyms m) => Pos -> [(AT.Ident, AT.Type)] -> AT.Ident -> m AT.Type
findMemTyp pos ms i = case lookup i ms of
    Nothing  -> errorWithPos pos $ noMember i
    Just typ -> pure typ

var :: (MonadReport Err m, HasSyms m) => Pos -> T.Ident -> m (AT.VarId, AT.Type)
var pos (T.Ident i) = getVar i >>= \case
    Just x  -> pure x
    Nothing -> errorWithPos pos $ undeclaredVariable i

arrElem :: (MonadReport Err m, HasSyms m) => Pos -> AT.Type -> T.Expr Pos -> m (AT.Expr, AT.Type)
arrElem pos arrTyp e = do
    elTyp <- case arrTyp of
        AT.Arr t -> pure t
        t        -> errorWithPos pos $ cannotIndex t
    (el, ixTyp) <- expr e
    unless (ixTyp == AT.Int) $
        errorWithPos pos $ indexMustBeInt ixTyp
    pure $ (el, elTyp)

attr :: (MonadReport Err m, HasSyms m) => Pos -> AT.Type -> T.LVal Pos -> m (AT.Attr, AT.Type)
attr pos strucTyp lv = case strucTyp of
    AT.Arr _ -> case lv of
        T.Var _ (T.Ident s) | s == "length" -> pure $ (AT.ALeaf s, AT.Int)
        _ -> errorWithPos pos $ illegalArrAttr
    AT.Struct _ ms -> case lv of
        T.Var pos (T.Ident s) -> do
            memTyp <- findMemTyp pos ms s
            pure (AT.ALeaf s, memTyp)
        T.ArrElem pos (T.Ident s) e -> do
            memTyp <- findMemTyp pos ms s
            (el, elTyp) <- arrElem pos memTyp e
            pure (AT.AArr s el, elTyp)
        T.Attr pos (T.Ident s) lv' -> do
            memTyp <- findMemTyp pos ms s
            first (AT.AStruct s) <$> attr pos memTyp lv'
    t -> errorWithPos pos $ typNotStruct t

lval :: (MonadReport Err m, HasSyms m) => T.LVal Pos -> m (AT.LVal, AT.Type)
lval (T.Var pos i) = first AT.Var <$> var pos i
lval (T.ArrElem pos i e) = do
    (a, arrTyp) <- var pos i
    (el, elTyp) <- arrElem pos arrTyp e
    pure (AT.ArrElem a el, elTyp)
lval (T.Attr pos i lv) = do
    (v, t) <- var pos i
    first (AT.Attr v) <$> attr pos t lv

expr :: (MonadReport Err m, HasSyms m) => T.Expr Pos -> m (AT.Expr, AT.Type)

expr (T.EVar _ lv) = first AT.EVar <$> lval lv

expr (T.ELitInt _ x) =
    pure (AT.ELitInt x, AT.Int)

expr (T.ELitTrue _) =
    pure (AT.ELitTrue, AT.Bool)

expr (T.ELitFalse _) =
    pure (AT.ELitFalse, AT.Bool)

expr (T.EApp pos (T.Ident str) es) = do
    aes <- mapM expr es
    AT.FunType retType argTypes <- getFun str >>= \case
        Just x  -> pure x
        Nothing -> errorWithPos pos $ unknownFun str
    let ts = map snd aes
    unless (argTypes == ts) $ errorWithPos pos $ argTypeMismatch argTypes ts
    pure (AT.EApp str (map fst aes), retType)

expr (T.ENewArr pos typ e) = do
    (e, lenTyp) <- expr e
    unless (lenTyp == AT.Int) $
        errorWithPos pos $ arrLengthMustBeInt lenTyp
    let typ' = annBType typ
    pure (AT.ENewArr e, AT.Arr typ')

expr (T.ENewStruc pos (T.Ident i)) = do
    typ@(AT.Struct _ ms) <- getStructType i >>= \case
        Just t  -> pure t
        Nothing -> errorWithPos pos $ unknownType i
    pure (AT.ENewStruct $ fromIntegral $ length ms, typ)

expr (T.ENull pos (T.Ident i)) = do
    typ <- getStructType i >>= \case
        Just t  -> pure t
        Nothing -> errorWithPos pos $ unknownType i
    pure (AT.ENull, typ)

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

cannotIndex :: AT.Type -> Err
cannotIndex t =
    "Cannot index a variable of type " <> show t

indexMustBeInt  :: AT.Type -> Err
indexMustBeInt t =
    "Array index must be of type int, not " <> show t

illegalArrAttr :: Err
illegalArrAttr =
    "Illegal attribute for array"

noMember :: AT.Ident -> Err
noMember i =
    "No member called" <> i

typNotStruct :: AT.Type -> Err
typNotStruct t =
    "Type " <> show t <> " has no members"

arrLengthMustBeInt :: AT.Type -> Err
arrLengthMustBeInt t =
    "Array length must be int, not " <> show t
