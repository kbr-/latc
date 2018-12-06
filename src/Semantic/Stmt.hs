{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Semantic.Stmt where

import qualified Annotated as AT
import qualified AbsLatte as T
import qualified Semantic.Expr as SE
import qualified Data.Map as M
import Data.Functor.Identity
import Data.Maybe
import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe

type ZS = StateT Env Identity

data Env = Env
    { funs :: M.Map String AT.FunType
    , scopeStack :: [Scope]
    , nextVarId :: Int
    , errors :: [ErrT]
    , funRetType :: AT.Type
    }

data Scope = Scope
    { vars :: M.Map AT.Ident (AT.VarId, AT.Type)
    }

type ErrT = String

pushScope :: ZS ()
pushScope = modify $ \e@Env{..} ->
    e { scopeStack = head scopeStack : scopeStack }

popScope :: ZS ()
popScope = modify $ \e@Env{..} ->
    e { scopeStack = tail scopeStack }

newVar :: AT.Type -> AT.Ident -> MaybeT ZS AT.VarId
newVar typ ident = do
    declared <- localDeclared ident . scopeStack <$> get
    when declared . reportError $ alreadyDeclared ident
    varId <- nextVarId <$> get
    currentScope <- head . scopeStack <$> get
    modify $ \e@Env{..} ->
        e { nextVarId = varId + 1
          , scopeStack = currentScope { vars = (M.insert ident (varId, typ) (vars currentScope)) } : tail scopeStack
          }
    pure varId
  where
    localDeclared :: String -> [Scope] -> Bool
    localDeclared str [s] = isJust . M.lookup ident $ vars s
    localDeclared str (s:s':_) = isJust (M.lookup ident (vars s)) && isNothing (M.lookup ident (vars s'))

getVar :: String -> MaybeT ZS (AT.VarId, AT.Type)
getVar str = vars . head . scopeStack <$> get >>= \vs ->
    case M.lookup str vs of
        Just x -> pure x
        Nothing -> reportError $ undefinedVariable str

getRetType :: ZS AT.Type
getRetType = funRetType <$> get

runExpr :: T.Expr -> MaybeT ZS (AT.Expr, AT.Type)
runExpr = MaybeT . runExpr'

runExpr' :: T.Expr -> ZS (Maybe (AT.Expr, AT.Type))
runExpr' e = get >>= \env@Env{..} ->
    case SE.runZE SE.Symbols { vars = vars (head scopeStack), funs = funs } (SE.expr e) of
        Left err -> put env { errors = err : errors } *> pure Nothing
        Right ae -> pure $ Just ae

liftMaybe :: Maybe a -> MaybeT ZS a
liftMaybe = MaybeT . pure

try :: MaybeT ZS AT.Stmt -> ZS AT.Stmt
try m = runMaybeT m >>= \z -> pure $ case z of
    Nothing   -> AT.Empty
    Just stmt -> stmt

reportError :: ErrT -> MaybeT ZS a
reportError err = modify (\env -> env { errors = err : errors env }) *> fail err

stmt :: T.Stmt -> ZS AT.Stmt

stmt T.Empty =
    pure AT.Empty

stmt (T.BStmt (T.Block stmts)) = do
    pushScope
    astmts <- mapM stmt stmts
    popScope
    pure $ AT.BStmt astmts

stmt (T.Decl typ items) = try $ do
    typ <- annType typ
    aitems <- MaybeT (sequence <$> mapM (runMaybeT . item typ) items)
    pure (AT.Decl typ aitems)
  where
    item :: AT.Type -> T.Item -> MaybeT ZS AT.Item
    item typ (T.NoInit (T.Ident ident)) = do
        varId <- newVar typ ident
        pure $ AT.NoInit (AT.LVal varId)
    item typ (T.Init (T.Ident ident) e) = do
        (ae, t) <- runExpr e
        unless (t == typ) . reportError $ typeMismatch t typ
        varId <- newVar typ ident
        pure $ AT.Init (AT.LVal varId) ae

    annType :: T.Type -> MaybeT ZS AT.Type
    annType T.Int = pure AT.Int
    annType T.Str = pure AT.Str
    annType T.Bool = pure AT.Bool
    annType typ = reportError $ incorrectVarType typ

stmt (T.Ass (T.Ident ident) e) = try $ do
    (ae, t) <- runExpr e
    (varId, typ) <- getVar ident
    unless (t == typ) . reportError $ typeMismatch t typ
    pure $ AT.Ass (AT.LVal varId) ae

stmt (T.Ret e) = try $ do
    (ae, t) <- runExpr e
    typ <- lift getRetType
    unless (t == typ) . reportError $ typeMismatch t typ
    pure $ AT.Ret ae

stmt T.VRet = try $ do
    typ <- lift getRetType
    unless (typ == AT.Void) . reportError $ mustReturnValue typ
    pure AT.VRet

stmt (T.Cond e s) =
    condStmt e $ (\as ae -> AT.Cond ae as) <$> stmt s

stmt (T.CondElse e s1 s2) =
    condStmt e $ (\as1 as2 ae -> AT.CondElse ae as1 as2) <$> stmt s1 <*> stmt s2

stmt (T.While e s) =
    condStmt e $ (\as ae -> AT.While ae as) <$> stmt s

stmt (T.SExp e) = try $ AT.SExp . fst <$> runExpr e

condStmt :: T.Expr -> ZS (AT.Expr -> AT.Stmt) -> ZS AT.Stmt
condStmt e mf = do
    mae <- runExpr' e
    f <- mf
    try $ do
        (ae, t) <- liftMaybe mae
        unless (t == AT.Bool) . reportError $ condExprNotBool t
        pure $ f ae

incorrectVarType :: T.Type -> ErrT
incorrectVarType = undefined

alreadyDeclared :: AT.Ident -> ErrT
alreadyDeclared = undefined

typeMismatch :: AT.Type -> AT.Type -> ErrT
typeMismatch = undefined

mustReturnValue :: AT.Type -> ErrT
mustReturnValue = undefined

condExprNotBool :: AT.Type -> ErrT
condExprNotBool = undefined

undefinedVariable :: AT.Ident -> errT
undefinedVariable = undefined
