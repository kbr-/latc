{-# LANGUAGE RecordWildCards #-}
module Semantic.Program where

import qualified Annotated as AT
import qualified AbsLatte as T
import qualified Semantic.Stmt as SS
import qualified Data.Map as M
import qualified Data.Set as S
import Semantic.ErrorT
import Data.Functor.Identity
import Data.Maybe
import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe

type Err = String

runProgram :: T.Program -> Either [Err] AT.Program
runProgram = runIdentity . runErrorT . program

type ZP = ErrorT Err Identity

program :: T.Program -> ZP AT.Program
program (T.Program defs) = do
    defs <- fromErrors . runDecls $ defs
    let funs = foldr (\FunDef{..} -> M.insert funIdent funType) M.empty defs
    runAll . map (def $ funs) $ defs

data FunDef = FunDef
    { funType  :: AT.FunType
    , funIdent :: AT.Ident
    , args     :: M.Map AT.VarId AT.ArgInfo
    , body     :: T.Block
    }

def :: M.Map AT.Ident AT.FunType -> FunDef -> ZP AT.TopDef
def funs FunDef{..} =
    (fromErrors . SS.runStmts env . fromBlock) body >>= \(body, locals) -> pure AT.FunDef{..}
  where
    env = SS.Env
        { funs = funs
        , scopeStack = [SS.Scope { vars = M.foldrWithKey insertArgVar M.empty args }]
        , nextVarId = (+1) . foldr max 0 . M.keys $ args
        , funRetType = retType funType
        , locals = fmap (\(AT.ArgInfo _ v) -> v) args
        }

    insertArgVar varId (AT.ArgInfo _ (AT.VarInfo ident typ)) = M.insert ident (varId, typ)
    retType (AT.FunType typ _) = typ
    fromBlock (T.Block b) = b

runDecls :: [T.TopDef] -> Either [Err] [FunDef]
runDecls = flip evalState S.empty . runErrorT . runAll . map decl

type ZD = ErrorT Err (State (S.Set AT.Ident))

decl :: T.TopDef -> ZD FunDef
decl (T.FnDef typ (T.Ident ident) args body) = do
    retType <- annType typ
    (argTypes, args) <- fromErrors . runArgs $ args
    declared <- S.member ident <$> get
    when declared . reportError $ funAlreadyDeclared ident
    modify $ S.insert ident
    pure FunDef
        { funType  = AT.FunType retType argTypes
        , funIdent = ident
        , args     = args
        , body     = body
        }

runArgs :: [T.Arg] -> Either [Err] ([AT.Type], M.Map AT.VarId AT.ArgInfo)
runArgs as = case flip runState env . runErrorT . runAll . map arg $ as of
    (Left errs, _)            -> throwError errs
    (Right types, ArgEnv{..}) -> pure (types, args')
  where
    env = ArgEnv { args' = M.empty, scope = M.empty, nextArg = 0 }

data ArgEnv = ArgEnv
    { args'   :: M.Map AT.VarId AT.ArgInfo
    , scope   :: M.Map AT.Ident AT.VarId
    , nextArg :: Int
    }

arg :: T.Arg -> ErrorT Err (State ArgEnv) AT.Type
arg (T.Arg typ (T.Ident ident)) = do
    typ <- annType typ
    declared <- M.member ident . scope <$> get
    when declared . reportError $ argAlreadyDeclared ident
    modify $ \e@ArgEnv{..} -> e
        { args'   = M.insert nextArg (AT.ArgInfo nextArg $ AT.VarInfo ident typ) args'
        , scope   = M.insert ident nextArg scope
        , nextArg = nextArg + 1
        }
    pure typ

annType :: T.Type -> ErrorT Err m AT.Type
annType = undefined

argAlreadyDeclared = undefined
funAlreadyDeclared = undefined
