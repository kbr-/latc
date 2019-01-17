{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Semantic.Common where

import qualified AbsLatte as T
import qualified Annotated as AT
import Control.Monad.Error.Class
import Data.Maybe
import Data.Monoid
import Semantic.ErrorT

type Pos = Maybe (Int, Int)

type Err = String

errorWithPos :: MonadReport Err m => Pos -> Err -> m a
errorWithPos pos err = reportError $ err <> posString pos

posString :: Pos -> String
posString (Just (line, col)) = "\nat line " <> show line <> ", column " <> show col
posString _                  = ""

annRetType :: MonadReport Err m => T.Type Pos -> m AT.Type
annRetType (T.Void _) = pure AT.Void
annRetType typ = annType typ

annType :: MonadReport Err m => T.Type Pos -> m AT.Type
annType (T.BType _ typ) = pure $ annBType typ
annType (T.Arr _ typ)   = pure $ AT.Arr $ annBType typ
annType (T.Void pos)    = errorWithPos pos $ invalidType "void"

annBType :: T.BType Pos -> AT.Type
annBType (T.Int _)  = AT.Int
annBType (T.Str _)  = AT.Str
annBType (T.Bool _) = AT.Bool

undeclaredVariable :: AT.Ident -> Err
undeclaredVariable ident = "Undeclared variable: " <> ident

invalidType :: String -> Err
invalidType t = "Invalid type: " <> t
