{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE MonoLocalBinds   #-}

module Language.Sonic.Compiler.Report
  ( MonadReport(..)
  , Severity(..)
  , Report
  )
where

import           GHC.Generics                   ( Generic )

import           Type.Membership                ( Forall
                                                , Member
                                                )

data Severity
  = Error
  | Warning
  | Note
  deriving (Eq, Show, Generic)

class Report a

class Forall Report reports => MonadReport reports m | m -> reports where
  report :: Member reports r => Severity -> r -> m ()
