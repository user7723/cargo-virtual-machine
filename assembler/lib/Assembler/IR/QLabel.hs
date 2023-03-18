module Assembler.IR.QLabel where

import Assembler.IR.Aliases
import Data.Word
import Data.Serialize
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

data QLabel = QLabel
  { labelQualifier :: ModuleName
  , labelSegType   :: SegType
  , labelName      :: Label
  , labelRelative  :: Maybe Label
  } deriving (Show, Eq, Ord)

instance Serialize QLabel where
  put QLabel{..} = do
    putListOf putText labelQualifier
    putWord8 $ segTypeToWord8 labelSegType
    putText labelName
    putMaybeOf putText labelRelative
    where
      putText = put . encodeUtf8

  get = do
    mn <- getListOf getText
    ls <- getWord8 >>= word8ToSegType
    ln <- getText
    ml <- getMaybeOf getText
    return $ QLabel mn ls ln ml
    where
      getText = get >>= return . decodeUtf8


data SegType
  = Data
  | Bss
  | Text
  | Local
  deriving (Show, Eq, Ord, Enum, Bounded)

segTypeToWord8 :: SegType -> Word8
segTypeToWord8 = fromIntegral . fromEnum

word8ToSegType :: Word8 -> Get SegType
word8ToSegType w
  |  w >= segTypeToWord8 minBound
  && w <= segTypeToWord8 maxBound = return $ toEnum $ fromIntegral w
  | otherwise = fail $ "invalid segment number: " ++ show w

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
