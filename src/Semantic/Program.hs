{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Semantic.Program where

import qualified Annotated as AT
import qualified AbsLatte as T
import qualified Semantic.Stmt as SS
import qualified Data.Map as M
import qualified Data.Set as S
import Semantic.ErrorT
import Semantic.Common
import Data.Functor.Identity
import Data.Maybe
import Data.Monoid
import Data.Either
import Control.Lens
import Control.Arrow
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe

predefFuns :: M.Map AT.Ident AT.FunType
predefFuns = M.fromList
    [("printInt",    AT.FunType AT.Void [AT.Int])
    ,("printString", AT.FunType AT.Void [AT.Str])
    ,("error",       AT.FunType AT.Void [])
    ,("readInt",     AT.FunType AT.Int  [])
    ,("readString",  AT.FunType AT.Str  [])
    ,("_concat",     AT.FunType AT.Str  [AT.Str, AT.Str])
    ,("_new",        AT.FunType (AT.Arr AT.Int) [AT.Int])
    ]

type Z = ErrorT Err Identity

data FunDef = FunDef
    { funType  :: AT.FunType
    , funIdent :: AT.Ident
    , funPos   :: Pos
    , args     :: M.Map AT.VarId AT.ArgInfo
    , body     :: [T.Stmt Pos]
    }

data StructDef = StructDef
    { structIdent :: AT.Ident
    , members     :: [(AT.Ident, T.Type Pos)]
    }

data Env = Env
    { funs    :: M.Map AT.Ident AT.FunType
    , structs :: M.Map AT.Ident AT.Type
    }

data ArgEnv = ArgEnv
    { _args'    :: M.Map AT.VarId AT.ArgInfo
    , _scope   :: M.Map AT.Ident AT.VarId
    , _nextArg :: Int
    }

instance HasTypes (ReaderT (M.Map AT.Ident AT.Type) Z) where
    getStructType i = reader $ M.lookup i

makeLenses ''ArgEnv

runProgram :: T.Program Pos -> Either [Err] AT.Program
runProgram = runIdentity . runErrorT . program

program :: T.Program Pos -> Z AT.Program
program (T.Program _ defs) = do
    structDefs <- collectStructs defs
    structs <- processStructs structDefs
    funDefs <- runReaderT (collectFuns defs) structs
    let funs = foldr (\FunDef{..} -> M.insert funIdent funType) predefFuns funDefs
    runAll . map (funDef Env{..}) $ funDefs

funDef :: Env -> FunDef -> Z AT.TopDef
funDef Env{..} FunDef{..} = do
    (body, locals) <- fromErrors . SS.runStmts env $ body
    when (funRetType /= AT.Void && not (alwaysReturn body)) $
        errorWithPos funPos $ mustReturn funIdent
    pure AT.FunDef{..}
  where
    env = SS.Env
        { funs       = funs
        , structs    = structs
        , scopeStack = [SS.Scope { vars = M.foldrWithKey insertArgVar M.empty args }]
        , nextVarId  = (+1) . foldr max (-1) . M.keys $ args
        , funRetType = funRetType
        , locals     = fmap (\(AT.ArgInfo _ v) -> v) args
        }

    funRetType = case funType of (AT.FunType typ _) -> typ
    insertArgVar varId (AT.ArgInfo _ (AT.VarInfo ident typ)) = M.insert ident (varId, typ)

collectStructs :: MonadReport Err m => [T.TopDef Pos] -> m [StructDef]
collectStructs xs = fmap catMaybes $ flip unwrap (flip evalState S.empty) $ runAll $ map collectStruct xs
  where
    collectStruct :: T.TopDef Pos -> ErrorT Err (State (S.Set AT.Ident)) (Maybe StructDef)
    collectStruct (T.FnDef _ _ _ _ _) = pure Nothing
    collectStruct (T.StructDef pos (T.Ident i) ms) = do
        declared <- S.member i <$> get
        when declared $
            errorWithPos pos $ structAlreadyDeclared i
        modify $ S.insert i
        ms <- collectMembers ms
        pure $ Just StructDef
            { structIdent = i
            , members = ms
            }

collectMembers :: MonadReport Err m => [T.Member Pos] -> m [(AT.Ident, T.Type Pos)]
collectMembers xs = flip evalStateT S.empty $ traverse collectMember xs
  where
    collectMember (T.Member pos typ (T.Ident i)) = do
        declared <- S.member i <$> get
        when declared $ errorWithPos pos $ memberAlreadyDeclared i
        modify $ S.insert i
        pure (i, typ)

processStructs :: MonadReport Err m => [StructDef] -> m (M.Map AT.Ident AT.Type)
processStructs xs = flip unwrap runIdentity (runAll (map (checkMembers . members) xs)) *> pure result
  where
    result = M.fromList result'
    result' = map (\StructDef{..} -> (structIdent, AT.Struct structIdent $ map processMember members)) xs
    processMember = second $ \case
        T.SType _ (T.Ident i) -> result M.! i
        T.BType _ typ         -> annBType typ
        T.Arr _ typ           -> AT.Arr $ annBType typ
    checkMembers = mapM_ checkMember
    checkMember (_, typ) = case typ of
        T.SType pos (T.Ident i) -> when (not $ S.member i idents) $ errorWithPos pos $ unknownType i
        T.Void pos              -> errorWithPos pos $ invalidType "void"
        _                       -> pure ()
    idents = S.fromList $ map structIdent xs

collectFuns :: (MonadReport Err m, HasTypes m) => [T.TopDef Pos] -> m [FunDef]
collectFuns xs = fmap catMaybes $ flip unwrap' (flip evalStateT S.empty) $ runAll $ map collectFun xs
  where
    collectFun (T.StructDef _ _ _) = pure Nothing
    collectFun (T.FnDef pos typ (T.Ident i) args (T.Block _ body)) = do
        retType <- lift . lift $ annRetType typ
        (argTypes, args) <- lift . lift $ runArgs args
        declared <- S.member i <$> get
        when declared $ errorWithPos pos $ funAlreadyDeclared i
        modify $ S.insert i
        pure $ Just FunDef
            { funType  = AT.FunType retType argTypes
            , funIdent = i
            , funPos   = pos
            , args     = args
            , body     = body
            }

runArgs :: (HasTypes m, MonadReport Err m) => [T.Arg Pos] -> m ([AT.Type], M.Map AT.VarId AT.ArgInfo)
runArgs as = fmap (second _args') $ flip runStateT env $ traverse runArg $ as
  where
    runArg (T.Arg pos typ (T.Ident i)) = do
        typ <- lift $ annType typ
        declared <- use $ scope . to (M.member i)
        when declared $ errorWithPos pos $ argAlreadyDeclared i

        currArg <- nextArg <<+= 1
        args' . at currArg ?= AT.ArgInfo currArg (AT.VarInfo i typ)
        scope . at i ?= currArg

        pure typ

    env = ArgEnv M.empty M.empty 0

alwaysReturn :: [AT.Stmt] -> Bool
alwaysReturn = foldr ((||) . alwaysReturns) False

alwaysReturns :: AT.Stmt -> Bool
alwaysReturns (AT.BStmt xs) = alwaysReturn xs
alwaysReturns (AT.Ret _) = True
alwaysReturns (AT.VRet) = True
alwaysReturns (AT.Cond e s) =
    case tryEvalBool e of
        Just True -> alwaysReturns s
        _         -> False
alwaysReturns (AT.CondElse e sThen sElse) =
    case tryEvalBool e of
        Just True  -> alwaysReturns sThen
        Just False -> alwaysReturns sElse
        _          -> alwaysReturns sThen && alwaysReturns sElse
alwaysReturns (AT.While e _) =
    case tryEvalBool e of
        Just True -> True -- actually infinite loop
        _         -> False
alwaysReturns (AT.SExp (AT.EApp "error" [])) = True
alwaysReturns _ = False

tryEvalBool :: AT.Expr -> Maybe Bool
tryEvalBool e = case tryEval e of
    Just (AT.ELitTrue)  -> Just True
    Just (AT.ELitFalse) -> Just False
    _                   -> Nothing

-- Evaluates an expression without variables and function applications.
-- Might evaluate differently in runtime due to integer overflows etc.
-- In this case the behavior is undefined.
tryEval :: AT.Expr -> Maybe AT.Expr
tryEval e@(AT.ELitInt _) = Just e
tryEval e@AT.ELitTrue = Just e
tryEval e@AT.ELitFalse = Just e
tryEval e@(AT.EString _) = Just e
tryEval (AT.Neg e) = (\(AT.ELitInt i) -> AT.ELitInt (-i)) <$> tryEval e
tryEval (AT.Not e) = flip fmap (tryEval e) $ \case
    AT.ELitTrue  -> AT.ELitFalse
    AT.ELitFalse -> AT.ELitTrue
tryEval (AT.EMul e1 op e2) = case (tryEval e1, tryEval e2) of
    (Just (AT.ELitInt x), Just (AT.ELitInt y)) -> Just . AT.ELitInt $ case op of
        AT.Times -> x * y
        AT.Div   -> x `div` y
        AT.Mod   -> x `mod` y
    _ -> Nothing
tryEval (AT.EAddInt e1 op e2) = case (tryEval e1, tryEval e2) of
    (Just (AT.ELitInt x), Just (AT.ELitInt y)) -> Just . AT.ELitInt $ case op of
        AT.Plus  -> x + y
        AT.Minus -> x - y
    _ -> Nothing
tryEval (AT.EAddString e1 e2) = case (tryEval e1, tryEval e2) of
    (Just (AT.EString x), Just (AT.EString y)) -> Just . AT.EString $ x ++ y
    _ -> Nothing
tryEval (AT.ERel e1 op e2) = case (tryEval e1, tryEval e2) of
    (Just (AT.ELitInt x), Just (AT.ELitInt y)) -> Just . boolToExpr $ case op of
        AT.LTH -> x < y
        AT.LE  -> x <= y
        AT.GTH -> x > y
        AT.GE  -> x >= y
        AT.EQU -> x == y
        AT.NE  -> x /= y
    (Just x, Just y) -> Just . boolToExpr $ case op of
        AT.EQU -> x == y
        AT.NE  -> x /= y
    _ -> Nothing
tryEval (AT.EAnd e1 e2) = case (tryEval e1, tryEval e2) of
    (Just x, Just y) -> Just . boolToExpr $ (exprToBool x) && (exprToBool y)
    _ -> Nothing
tryEval (AT.EOr e1 e2) = case (tryEval e1, tryEval e2) of
    (Just x, Just y) -> Just . boolToExpr $ (exprToBool x) || (exprToBool y)
    _ -> Nothing
tryEval _ = Nothing

exprToBool :: AT.Expr -> Bool
exprToBool AT.ELitTrue  = True
exprToBool AT.ELitFalse = False

boolToExpr :: Bool -> AT.Expr
boolToExpr True  = AT.ELitTrue
boolToExpr False = AT.ELitFalse

argAlreadyDeclared :: AT.Ident -> Err
argAlreadyDeclared ident =
    "Argument already declared: " <> ident

funAlreadyDeclared :: AT.Ident -> Err
funAlreadyDeclared ident =
    "Function already declared: " <> ident <>
    if elem ident (map fst . M.toList $ predefFuns) then " (builtin function)" else ""

structAlreadyDeclared :: AT.Ident -> Err
structAlreadyDeclared  i =
    "Structure already declared: " <> i

mustReturn :: AT.Ident -> Err
mustReturn ident =
    "Could not deduce that function " <> ident <> " always returns"

memberAlreadyDeclared :: AT.Ident -> Err
memberAlreadyDeclared i =
    "Member already declared: " <> i
