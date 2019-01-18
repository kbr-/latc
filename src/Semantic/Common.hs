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

class HasTypes m where
    getStructType :: AT.Ident -> m (Maybe AT.Type)

errorWithPos :: MonadReport Err m => Pos -> Err -> m a
errorWithPos pos err = reportError $ err <> posString pos

posString :: Pos -> String
posString (Just (line, col)) = "\nat line " <> show line <> ", column " <> show col
posString _                  = ""

annRetType :: (MonadReport Err m, HasTypes m) => T.Type Pos -> m AT.Type
annRetType = \case
    T.Void _ -> pure AT.Void
    typ      -> annType typ

annType :: (MonadReport Err m, HasTypes m) => T.Type Pos -> m AT.Type
annType = \case
    T.BType _ typ           -> pure $ annBType typ
    T.Arr _ typ             -> pure $ AT.Arr $ annBType typ
    T.Void pos              -> errorWithPos pos $ invalidType "void"
    T.SType pos (T.Ident i) -> getStructType i >>= \case
        Just typ -> pure typ
        _        -> errorWithPos pos $ unknownType i

annBType :: T.BType Pos -> AT.Type
annBType = \case
    T.Int _  -> AT.Int
    T.Str _  -> AT.Str
    T.Bool _ -> AT.Bool

undeclaredVariable :: AT.Ident -> Err
undeclaredVariable ident = "Undeclared variable: " <> ident

invalidType :: String -> Err
invalidType t = "Invalid type: " <> t

unknownType :: AT.Ident -> Err
unknownType i =
    "Unknown type: " <> i
