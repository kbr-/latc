{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
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
import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe

runProgram :: T.Program Pos -> Either [Err] AT.Program
runProgram = runIdentity . runErrorT . program

predefFuns :: M.Map AT.Ident AT.FunType
predefFuns = M.fromList
    [("printInt",    AT.FunType AT.Void [AT.Int])
    ,("printString", AT.FunType AT.Void [AT.Str])
    ,("error",       AT.FunType AT.Void [])
    ,("readInt",     AT.FunType AT.Int  [])
    ,("readString",  AT.FunType AT.Str  [])
    ]

type ZP = ErrorT Err Identity

program :: T.Program Pos -> ZP AT.Program
program (T.Program _ defs) = do
    defs <- fromErrors . runDecls $ defs
    let funs = foldr (\FunDef{..} -> M.insert funIdent funType) predefFuns defs
    runAll . map (def funs) $ defs

data FunDef = FunDef
    { funType  :: AT.FunType
    , funIdent :: AT.Ident
    , funPos   :: Pos
    , args     :: M.Map AT.VarId AT.ArgInfo
    , body     :: T.Block Pos
    }

def :: M.Map AT.Ident AT.FunType -> FunDef -> ZP AT.TopDef
def funs FunDef{..} = do
    (body, locals) <- fromErrors . SS.runStmts env . fromBlock $ body
    when (funRetType /= AT.Void && not (alwaysReturn body)) $
        reportErrorWithPos funPos $ mustReturn funIdent
    pure AT.FunDef{..}
  where
    env = SS.Env
        { funs = funs
        , scopeStack = [SS.Scope { vars = M.foldrWithKey insertArgVar M.empty args }]
        , nextVarId = (+1) . foldr max (-1) . M.keys $ args
        , funRetType = funRetType
        , locals = fmap (\(AT.ArgInfo _ v) -> v) args
        }

    funRetType = case funType of (AT.FunType typ _) -> typ
    insertArgVar varId (AT.ArgInfo _ (AT.VarInfo ident typ)) = M.insert ident (varId, typ)
    fromBlock (T.Block _ b) = b

runDecls :: [T.TopDef Pos] -> Either [Err] [FunDef]
runDecls = flip evalState (M.keysSet predefFuns) . runErrorT . runAll . map decl

type ZD = ErrorT Err (State (S.Set AT.Ident))

decl :: T.TopDef Pos -> ZD FunDef
decl (T.FnDef pos typ (T.Ident ident) args body) = do
    retType <- annRetType typ
    (argTypes, args) <- fromErrors . runArgs $ args
    declared <- S.member ident <$> get
    when declared . reportErrorWithPos pos $ funAlreadyDeclared ident
    modify $ S.insert ident
    pure FunDef
        { funType  = AT.FunType retType argTypes
        , funIdent = ident
        , funPos   = pos
        , args     = args
        , body     = body
        }

runArgs :: [T.Arg Pos] -> Either [Err] ([AT.Type], M.Map AT.VarId AT.ArgInfo)
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

arg :: T.Arg Pos -> ErrorT Err (State ArgEnv) AT.Type
arg (T.Arg pos typ (T.Ident ident)) = do
    typ <- annType typ
    declared <- M.member ident . scope <$> get
    when declared . reportErrorWithPos pos $ argAlreadyDeclared ident
    modify $ \e@ArgEnv{..} -> e
        { args'   = M.insert nextArg (AT.ArgInfo nextArg $ AT.VarInfo ident typ) args'
        , scope   = M.insert ident nextArg scope
        , nextArg = nextArg + 1
        }
    pure typ

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
    "Function already declared: " <> ident

mustReturn :: AT.Ident -> Err
mustReturn ident =
    "Could not deduce that function " <> ident <> " always returns"
