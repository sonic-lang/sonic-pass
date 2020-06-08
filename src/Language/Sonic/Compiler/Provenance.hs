module Language.Sonic.Compiler.Provenance
  ( WithProv(..)
  , Prov(..)
  , Source(..)
  )
where

import           GHC.Generics                   ( Generic
                                                , Generic1
                                                )

import           Language.Sonic.Compiler.Pass   ( Pass )
import           Language.Sonic.Compiler.Location
                                                ( Location )
import           Language.Sonic.Compiler.IR.Tree
                                                ( Unwrap(..) )

-- | Wrap @a@ with 'Prov'.
data WithProv a = WithProv Prov a
  deriving (Eq, Show, Generic, Generic1, Functor, Foldable, Traversable)

instance Unwrap WithProv where
  unwrap (WithProv _ x) = x

data Prov
  = Source Source
  | Derived Pass Prov
  deriving (Eq, Show, Generic)

data Source
  = Parsed Location
  | Generated Pass
  deriving (Eq, Show, Generic)
