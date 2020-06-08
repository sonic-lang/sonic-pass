module Language.Sonic.Compiler.Unique
  ( MonadUnique(..)
  , Unique
  , unique
  )
where

class Monad m => MonadUnique m where
  uniqueInt :: m Int

newtype Unique = MkUnique Int
  deriving (Eq, Show, Ord)

unique :: MonadUnique m => m Unique
unique = MkUnique <$> uniqueInt
