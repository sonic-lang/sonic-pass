{-# LANGUAGE OverloadedStrings #-}

module Language.Sonic.Compiler.Path.Constant
  ( tupleCtor
  , tupleTyCtor
  )
where

import           Data.Text                      ( pack )

import           Language.Sonic.Compiler.IR.EntityKind
                                                ( Ctor
                                                , TyCtor
                                                )
import           Language.Sonic.Compiler.Path   ( Path(..)
                                                , PathPrefix(..)
                                                , SimplePath(..)
                                                , Name(..)
                                                )

tupleCtor :: Int -> Path Ctor
tupleCtor n = Path (Just Global) p
 where
  p     = SimplePath (Just tuple) ctor
  ctor  = Name ("Tuple" <> pack (show n))
  tuple = SimplePath (Just std) (Name "tuple")
  std   = SimplePath Nothing (Name "std")

tupleTyCtor :: Int -> Path TyCtor
tupleTyCtor n = Path (Just Global) p
 where
  p     = SimplePath (Just tuple) ctor
  ctor  = Name ("Tuple" <> pack (show n))
  tuple = SimplePath (Just std) (Name "tuple")
  std   = SimplePath Nothing (Name "std")
