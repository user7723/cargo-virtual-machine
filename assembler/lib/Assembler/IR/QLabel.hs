module Assembler.IR.QLabel where

import Assembler.IR.Aliases
import Data.Word

data QLabel = QLabel
  { labelQualifier :: ModuleName
  , labelSegType   :: SegType
  , labelName      :: Label
  , labelRelative  :: Maybe Label
  } deriving (Show, Eq, Ord)

data SegType
  = Data
  | Bss
  | Text
  | Local
  deriving (Show, Eq, Ord, Enum, Bounded)

data Access = LocalScope | Readable | Executable
  deriving Show

numberToSegType :: Access -> Word64 -> Maybe SegType
numberToSegType a n
  | check n   = Just $ toEnum $ fromIntegral n
  | otherwise = Nothing
  where
    minS = fromIntegral $ fromEnum (minBound :: SegType)
    maxS = fromIntegral $ fromEnum (maxBound :: SegType)
    check x = all ($ x) [(>= minS), (<= maxS), accessMatch]
    accessMatch = case a of
      Executable -> (== textIdx)
      _          -> (/= textIdx)
    textIdx = fromIntegral $ fromEnum Text
