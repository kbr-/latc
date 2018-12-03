{-# LANGUAGE LambdaCase #-}
module Semantic.Stmt where

import qualified Annotated as AT
import qualified AbsLatte as T
import qualified Data.Map as M
import Data.Functor.Identity
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
    }

data Scope = Scope
    { vars :: M.Map AT.Ident (AT.VarId, AT.Type)
    }

type ErrT = String

pushScope :: ZS ()
pushScope = undefined

popScope :: ZS ()
popScope = undefined

newVar :: AT.Type -> AT.Ident -> MaybeT ZS AT.VarId
newVar typ ident = do
    declared <- localDeclared ident
    when declared $ (lift . reportError) (alreadyDeclared ident) *> fail ""
    undefined
  where
    localDeclared = undefined

runExpr :: T.Expr -> MaybeT ZS (AT.Expr, AT.Type)
runExpr = undefined

try :: MaybeT ZS AT.Stmt -> ZS AT.Stmt
try m = runMaybeT m >>= \z -> pure $ case z of
    Nothing   -> AT.Empty
    Just stmt -> stmt

annType :: T.Type -> MaybeT ZS AT.Type
annType typ = (lift . reportError) (incorrectVarType typ) *> fail ""

reportError :: ErrT -> ZS ()
reportError = undefined

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
        unless (t == typ) $ (lift . reportError) (typeMismatch t typ) *> fail ""
        varId <- newVar typ ident
        pure $ AT.Init (AT.LVal varId) ae

incorrectVarType :: T.Type -> ErrT
incorrectVarType = undefined

alreadyDeclared :: AT.Ident -> ErrT
alreadyDeclared = undefined

typeMismatch :: AT.Type -> AT.Type -> ErrT
typeMismatch = undefined
