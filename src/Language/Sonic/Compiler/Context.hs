module Language.Sonic.Compiler.Context
  ( FileContext(..)
  )
where

class Monad m => FileContext m where
  currentFile :: m FilePath
