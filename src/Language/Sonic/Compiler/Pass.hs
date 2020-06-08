{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE ExistentialQuantification #-}

module Language.Sonic.Compiler.Pass
  ( Pass
  , pass
  , PassIndex(..)
  )
where

import           Data.Maybe                     ( isJust )
import           Type.Reflection                ( TypeRep
                                                , Typeable
                                                , typeRep
                                                , eqTypeRep
                                                )

class Typeable a => PassIndex a where
  name :: proxy a -> String

data Pass = forall a. PassIndex a => Pass (TypeRep a)

pass :: forall a proxy . PassIndex a => proxy a -> Pass
pass _ = Pass (typeRep :: TypeRep a)

instance Eq Pass where
  Pass a == Pass b = isJust $ eqTypeRep a b

instance Show Pass where
  show (Pass rep) = name rep
