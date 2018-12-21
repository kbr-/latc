{-# LANGUAGE FlexibleContexts #-}
module Semantic.Common where

import qualified AbsLatte as T
import qualified Annotated as AT
import Control.Monad.Error.Class
import Data.Maybe
import Data.Monoid
import Semantic.ErrorT

type Pos = Maybe (Int, Int)

type Err = String

errorWithPos :: MonadError Err m => Pos -> Err -> m a
errorWithPos pos err = throwError $ err <> posString pos

reportErrorWithPos :: Monad m => Pos -> Err -> ErrorT Err m a
reportErrorWithPos pos err = reportError $ err <> posString pos

posString :: Pos -> String
posString (Just (line, col)) = "\nat line " <> show line <> ", column " <> show col
posString _                  = ""

annRetType :: Monad m => T.Type Pos -> ErrorT Err m AT.Type
annRetType (T.Void _) = pure AT.Void
annRetType typ = annType typ

annType :: Monad m => T.Type Pos -> ErrorT Err m AT.Type
annType (T.Int _)           = pure AT.Int
annType (T.Str _)           = pure AT.Str
annType (T.Bool _)          = pure AT.Bool
annType typ@(T.Void pos)    = reportErrorWithPos pos $ invalidType "void"
annType typ@(T.Fun pos _ _) = reportErrorWithPos pos $ invalidType "function type"

undeclaredVariable :: AT.Ident -> Err
undeclaredVariable ident = "Undeclared variable: " <> ident

invalidType :: String -> Err
invalidType t = "Invalid type: " <> t
