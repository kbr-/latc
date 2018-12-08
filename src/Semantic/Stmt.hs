{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Semantic.Stmt where

import qualified Annotated as AT
import qualified AbsLatte as T
import qualified Semantic.Expr as SE
import qualified Data.Map as M
import Semantic.ErrorT
import Data.Functor.Identity
import Data.Maybe
import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe

type Err = String

type ZS = ErrorT Err (State Env)

data Env = Env
    { funs :: M.Map String AT.FunType
    , scopeStack :: [Scope]
    , nextVarId :: Int
    , funRetType :: AT.Type
    , locals :: M.Map AT.VarId AT.VarInfo
    }

data Scope = Scope
    { vars :: M.Map AT.Ident (AT.VarId, AT.Type)
    }

runStmts :: Env -> [T.Stmt] -> Either [Err] ([AT.Stmt], M.Map AT.VarId AT.VarInfo)
runStmts env ss = case flip runState env . runErrorT . runAll . map stmt $ ss of
    (Left errs, _)     -> throwError errs
    (Right x, Env{..}) -> pure (x, locals)

pushScope :: ZS ()
pushScope = modify $ \e@Env{..} ->
    e { scopeStack = head scopeStack : scopeStack }

popScope :: ZS ()
popScope = modify $ \e@Env{..} ->
    e { scopeStack = tail scopeStack }

newVar :: AT.Type -> AT.Ident -> ZS AT.VarId
newVar typ ident = do
    declared <- localDeclared ident . scopeStack <$> get
    when declared . reportError $ alreadyDeclared ident
    varId <- nextVarId <$> get
    currentScope <- head . scopeStack <$> get
    modify $ \e@Env{..} ->
        e { nextVarId = varId + 1
          , scopeStack = currentScope { vars = (M.insert ident (varId, typ) (vars currentScope)) } : tail scopeStack
          , locals = M.insert varId (AT.VarInfo ident typ) locals
          }
    pure varId
  where
    localDeclared :: String -> [Scope] -> Bool
    localDeclared str [s] = M.member str $ vars s
    localDeclared str (s:s':_) = M.member str (vars s) && (M.notMember str (vars s'))

getVar :: String -> ZS (AT.VarId, AT.Type)
getVar str = vars . head . scopeStack <$> get >>= \vs ->
    case M.lookup str vs of
        Just x  -> pure x
        Nothing -> reportError $ undefinedVariable str

getRetType :: ZS AT.Type
getRetType = funRetType <$> get

runExpr :: T.Expr -> ZS (AT.Expr, AT.Type)
runExpr e = get >>= \Env{..} ->
    fromError . SE.runZE SE.Symbols { vars = vars (head scopeStack), funs = funs } $ SE.expr e

stmt :: T.Stmt -> ZS AT.Stmt

stmt T.Empty =
    pure AT.Empty

stmt (T.BStmt (T.Block stmts)) = do
    pushScope
    astmts <- runAll . map stmt $ stmts
    popScope
    pure $ AT.BStmt astmts

stmt (T.Decl typ items) = do
    typ <- annType typ
    items <- runAll . map (item typ) $ items
    pure (AT.Decl typ items)
  where
    item :: AT.Type -> T.Item -> ZS AT.Item
    item typ (T.NoInit (T.Ident ident)) = do
        varId <- newVar typ ident
        pure $ AT.NoInit (AT.LVal varId)
    item typ (T.Init (T.Ident ident) e) = do
        (ae, t) <- runExpr e
        unless (t == typ) . reportError $ typeMismatch t typ
        varId <- newVar typ ident
        pure $ AT.Init (AT.LVal varId) ae

    annType :: T.Type -> ZS AT.Type
    annType T.Int = pure AT.Int
    annType T.Str = pure AT.Str
    annType T.Bool = pure AT.Bool
    annType typ = reportError $ incorrectVarType typ

stmt (T.Ass (T.Ident ident) e) = do
    (ae, t) <- runExpr e
    (varId, typ) <- getVar ident
    unless (t == typ) . reportError $ typeMismatch t typ
    pure $ AT.Ass (AT.LVal varId) ae

stmt (T.Ret e) = do
    (ae, t) <- runExpr e
    typ <- getRetType
    unless (t == typ) . reportError $ typeMismatch t typ
    pure $ AT.Ret ae

stmt T.VRet = do
    typ <- getRetType
    unless (typ == AT.Void) . reportError $ mustReturnValue typ
    pure AT.VRet

stmt (T.Cond e s) =
    condStmt e $ (\as ae -> AT.Cond ae as) <$> stmt s

stmt (T.CondElse e s1 s2) =
    condStmt e $ (\as1 as2 ae -> AT.CondElse ae as1 as2) <$> stmt s1 <*> stmt s2

stmt (T.While e s) =
    condStmt e $ (\as ae -> AT.While ae as) <$> stmt s

stmt (T.SExp e) = AT.SExp . fst <$> runExpr e

condStmt :: T.Expr -> ZS (AT.Expr -> AT.Stmt) -> ZS AT.Stmt
condStmt e mf = do
    ((ae, t), f) <- run2 (runExpr e) mf
    unless (t == AT.Bool) . reportError $ condExprNotBool t
    pure $ f ae

incorrectVarType :: T.Type -> Err
incorrectVarType = undefined

alreadyDeclared :: AT.Ident -> Err
alreadyDeclared = undefined

typeMismatch :: AT.Type -> AT.Type -> Err
typeMismatch = undefined

mustReturnValue :: AT.Type -> Err
mustReturnValue = undefined

condExprNotBool :: AT.Type -> Err
condExprNotBool = undefined

undefinedVariable :: AT.Ident -> Err
undefinedVariable = undefined
