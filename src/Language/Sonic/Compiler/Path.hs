{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds      #-}

module Language.Sonic.Compiler.Path
  ( PathPrefix(..)
  , Path(..)
  , plainPath
  , localPath
  , SimplePath(..)
  , Name(..)
  , newName
  )
where

import           GHC.Generics                   ( Generic )
import           Data.Data                      ( Data )
import           Data.Text                      ( Text )

import           Language.Sonic.Compiler.IR.EntityKind
                                                ( EntityKind
                                                , Module
                                                )
import           Language.Sonic.Compiler.Unique ( MonadUnique
                                                , Unique
                                                , unique
                                                )

data PathPrefix
  = Global
  | Local
  | Primitive
  deriving (Show, Eq, Generic, Data)

data Path (k :: EntityKind)
  = Path
  { prefix :: Maybe PathPrefix
  , inner  :: SimplePath k
  }
  deriving (Show, Eq, Generic)

data SimplePath (k :: EntityKind)
  = SimplePath
  { parent :: Maybe (SimplePath Module)
  , name   :: Name k
  }
  deriving (Show, Eq, Generic)

plainPath :: Name k -> Path k
plainPath = Path Nothing . SimplePath Nothing

localPath :: Name k -> Path k
localPath = Path (Just Local) . SimplePath Nothing

data Name (k :: EntityKind)
  = Name Text
  | Unique Unique
  deriving (Show, Eq, Generic)

newName :: MonadUnique m => m (Name k)
newName = Unique <$> unique
