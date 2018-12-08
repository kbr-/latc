{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
-- Processing multiple items that may fail and keeping all the errors.

module Semantic.ErrorT where

import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Control.Arrow

newtype ErrorT e m a = ErrorT (MaybeT (StateT [e] m) a)
    deriving (Functor, Applicative, Monad)

instance MonadTrans (ErrorT e) where
    lift = ErrorT . lift . lift

instance MonadState s m => MonadState s (ErrorT e m) where
    get = lift get
    put = lift . put
    state = lift . state

runErrorT :: Functor m => ErrorT e m a -> m (Either [e] a)
runErrorT (ErrorT m) = fmap mkEither . flip runStateT [] . runMaybeT $ m
  where
    mkEither (Just x, _) = Right x
    mkEither (_, es)     = Left es

runAll :: (Monad m, Traversable t) => t (ErrorT e m a) -> ErrorT e m (t a)
runAll = ErrorT . MaybeT . fmap sequence . traverse (\(ErrorT m) -> runMaybeT m)

run2 :: Monad m => ErrorT e m a -> ErrorT e m b -> ErrorT e m (a, b)
run2 (ErrorT ma) (ErrorT mb) = ErrorT . MaybeT $
    runMaybeT ma >>= \a -> runMaybeT mb >>= \b -> pure $ (,) <$> a <*> b

fromError :: Monad m => Either e a -> ErrorT e m a
fromError = fromErrors . left pure

fromErrors :: Monad m => Either [e] a -> ErrorT e m a
fromErrors (Left e)  = reportErrors e
fromErrors (Right x) = pure x

reportError :: Monad m => e -> ErrorT e m a
reportError e = reportErrors [e]

reportErrors :: Monad m => [e] -> ErrorT e m a
reportErrors e = ErrorT $ mapM (modify . (:)) e *> fail ""
