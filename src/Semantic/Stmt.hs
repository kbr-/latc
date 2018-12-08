{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Semantic.Stmt where

import qualified Annotated as AT
import qualified AbsLatte as T
import qualified Semantic.Expr as SE
import qualified Data.Map as M
import Semantic.Common
import Semantic.ErrorT
import Data.Functor.Identity
import Data.Maybe
import Data.Monoid
import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe

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

runStmts :: Env -> [T.Stmt Pos] -> Either [Err] ([AT.Stmt], M.Map AT.VarId AT.VarInfo)
runStmts env ss = case flip runState env . runErrorT . runAll . map stmt $ ss of
    (Left errs, _)     -> throwError errs
    (Right x, Env{..}) -> pure (x, locals)

pushScope :: ZS ()
pushScope = modify $ \e@Env{..} ->
    e { scopeStack = head scopeStack : scopeStack }

popScope :: ZS ()
popScope = modify $ \e@Env{..} ->
    e { scopeStack = tail scopeStack }

newVar :: Pos -> AT.Type -> AT.Ident -> ZS AT.VarId
newVar pos typ ident = do
    declared <- localDeclared ident . scopeStack <$> get
    when declared . reportErrorWithPos pos $ alreadyDeclared ident
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

getVar :: Pos -> String -> ZS (AT.VarId, AT.Type)
getVar pos str = vars . head . scopeStack <$> get >>= \vs ->
    case M.lookup str vs of
        Just x  -> pure x
        Nothing -> reportErrorWithPos pos $ undeclaredVariable str

getRetType :: ZS AT.Type
getRetType = funRetType <$> get

runExpr :: T.Expr Pos -> ZS (AT.Expr, AT.Type)
runExpr e = get >>= \Env{..} ->
    fromError . SE.runZE SE.Symbols { vars = vars (head scopeStack), funs = funs } $ SE.expr e

stmt :: T.Stmt Pos -> ZS AT.Stmt

stmt (T.Empty _) =
    pure AT.Empty

stmt (T.BStmt _ (T.Block _ stmts)) = do
    pushScope
    astmts <- runAll . map stmt $ stmts
    popScope
    pure $ AT.BStmt astmts

stmt (T.Decl _ typ items) = do
    typ <- annType typ
    items <- runAll . map (item typ) $ items
    pure (AT.Decl typ items)
  where
    item :: AT.Type -> T.Item Pos -> ZS AT.Item
    item typ (T.NoInit pos (T.Ident ident)) = do
        varId <- newVar pos typ ident
        pure $ AT.NoInit (AT.LVal varId)
    item typ (T.Init pos (T.Ident ident) e) = do
        (ae, t) <- runExpr e
        unless (t == typ) . reportErrorWithPos pos $ varTypeMismatch t typ
        varId <- newVar pos typ ident
        pure $ AT.Init (AT.LVal varId) ae

stmt (T.Ass pos (T.Ident ident) e) = do
    (ae, t) <- runExpr e
    (varId, typ) <- getVar pos ident
    unless (t == typ) . reportErrorWithPos pos $ varTypeMismatch t typ
    pure $ AT.Ass (AT.LVal varId) ae

stmt (T.Incr pos (T.Ident ident)) = do
    (varId, typ) <- getVar pos ident
    unless (typ == AT.Int) . reportErrorWithPos pos $ cannotIncr typ
    pure $ AT.Incr (AT.LVal varId)

stmt (T.Decr pos (T.Ident ident)) = do
    (varId, typ) <- getVar pos ident
    unless (typ == AT.Int) . reportErrorWithPos pos $ cannotDecr typ
    pure $ AT.Decr (AT.LVal varId)

stmt (T.Ret pos e) = do
    (ae, t) <- runExpr e
    typ <- getRetType
    unless (t == typ) . reportErrorWithPos pos $ retTypeMismatch t typ
    pure $ AT.Ret ae

stmt (T.VRet pos) = do
    typ <- getRetType
    unless (typ == AT.Void) . reportErrorWithPos pos $ mustReturnValue typ
    pure AT.VRet

stmt (T.Cond pos e s) =
    condStmt pos e $ (\as ae -> AT.Cond ae as) <$> stmt s

stmt (T.CondElse pos e s1 s2) =
    condStmt pos e $ (\as1 as2 ae -> AT.CondElse ae as1 as2) <$> stmt s1 <*> stmt s2

stmt (T.While pos e s) =
    condStmt pos e $ (\as ae -> AT.While ae as) <$> stmt s

stmt (T.SExp _ e) = AT.SExp . fst <$> runExpr e

condStmt :: Pos -> T.Expr Pos -> ZS (AT.Expr -> AT.Stmt) -> ZS AT.Stmt
condStmt pos e mf = do
    ((ae, t), f) <- run2 (runExpr e) mf
    unless (t == AT.Bool) . reportErrorWithPos pos $ condExprNotBool t
    pure $ f ae

alreadyDeclared :: AT.Ident -> Err
alreadyDeclared ident =
    "Variable \"" <> ident <> "\" already declared"

cannotIncr :: AT.Type -> Err
cannotIncr t =
    "Cannot increment variable of type " <> show t

cannotDecr :: AT.Type -> Err
cannotDecr t =
    "Cannot decrement variable of type " <> show t

varTypeMismatch :: AT.Type -> AT.Type -> Err
varTypeMismatch exprTyp varTyp =
    "Type " <> show varTyp <> " of variable doesn't match type " <> show exprTyp <> " of expression"

retTypeMismatch :: AT.Type -> AT.Type -> Err
retTypeMismatch exprTyp retTyp =
    "Type " <> show retTyp <> " returned from function doesn't match\
    \ type " <> show exprTyp <> " of expression"

mustReturnValue :: AT.Type -> Err
mustReturnValue t =
    "Invalid void return: function has return type " <> show t <> ""

condExprNotBool :: AT.Type -> Err
condExprNotBool t =
    "Type of conditional expression must be " <> show AT.Bool <> ", not " <> show t
