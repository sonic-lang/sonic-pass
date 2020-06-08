module Language.Sonic.Compiler.Location
  ( Location(..)
  , Span(..)
  , Position(..)
  , Line(..)
  , Column(..)
  )
where

import           GHC.Generics                   ( Generic )
import           Data.Data                      ( Data )
import           Numeric.Natural                ( Natural )

newtype Line = Line Natural
  deriving (Eq, Show, Ord, Generic, Data)

newtype Column = Column Natural
  deriving (Eq, Show, Ord, Generic, Data)

data Position
  = Position
  { line   :: Line
  , column :: Column
  }
  deriving (Eq, Show, Generic, Data)

data Span
  = Span
  { begin :: Position
  , end   :: Position
  }
  deriving (Eq, Show, Generic, Data)

data Location
  = Location
  { file :: FilePath
  , span :: Span
  }
  deriving (Eq, Show, Generic, Data)
