{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
-- Processing multiple items that may fail and keeping all the errors.

module Semantic.ErrorT where

import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Control.Monad.Except
import Control.Monad.Reader
import Control.Arrow

newtype ErrorT e m a = ErrorT (MaybeT (StateT [e] m) a)
    deriving (Functor, Applicative, Monad)

instance MonadTrans (ErrorT e) where
    lift = ErrorT . lift . lift

instance MonadState s m => MonadState s (ErrorT e m) where
    get = lift get
    put = lift . put
    state = lift . state

instance MonadReader r m => MonadReader r (ErrorT e m) where
    ask = lift ask
    local = mapErrorT . local
    reader = lift . reader

class Monad m => MonadReport e m | m -> e where
    reportErrors :: [e] -> m a

instance Monad m => MonadReport e (ErrorT e m) where
    reportErrors e = ErrorT $ mapM (modify . (:)) e *> fail ""

instance MonadReport e m => MonadReport e (StateT s m) where
    reportErrors = lift . reportErrors

instance MonadReport e m => MonadReport e (ReaderT r m) where
    reportErrors = lift . reportErrors

reportError :: MonadReport e m => e -> m a
reportError = reportErrors . pure

mapErrorT f (ErrorT m) = ErrorT $ mapMaybeT f m

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

fromError :: MonadReport e m => Either e a -> m a
fromError = fromErrors . left pure

fromErrors :: MonadReport e m => Either [e] a -> m a
fromErrors (Left e)  = reportErrors e
fromErrors (Right x) = pure x

unwrap :: (Monad m, MonadReport e n) => ErrorT e m a -> (m (Either [e] a) -> Either [e] a) -> n a
unwrap m f = fromErrors $ f (runErrorT m)

unwrap' :: (Monad m, MonadReport e n) => ErrorT e m a -> (m (Either [e] a) -> n (Either [e] a)) -> n a
unwrap' m f = join $ fromErrors <$> f (runErrorT m)
