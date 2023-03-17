module Assembler.IR.StaticMemory where

import Data.Word

data AllocDirective
  = AllocVar
  | AllocArr ArrayType Word64
  deriving Show

data InitDirective
  = InitVar           Word64
  | InitArr ArrayType [Word64]
  deriving Show

data ArrayType
  = A8
  | A16
  | A32
  | A64
  deriving (Enum, Bounded)

instance Show ArrayType where
  show A8  = "8"
  show A16 = "16"
  show A32 = "32"
  show A64 = "64"

elemCapacity :: Num a => ArrayType -> a
elemCapacity A8  = fromIntegral (maxBound :: Word8)
elemCapacity A16 = fromIntegral (maxBound :: Word16)
elemCapacity A32 = fromIntegral (maxBound :: Word32)
elemCapacity A64 = fromIntegral (maxBound :: Word64)
