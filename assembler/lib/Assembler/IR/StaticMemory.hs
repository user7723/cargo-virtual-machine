module Assembler.IR.StaticMemory where

import Data.Word
import Data.Serialize

data AllocDirective
  = AllocVar
  | AllocArr ArrayType Word64
  deriving Show

allocVarMask :: Word8
allocVarMask = 0b1111_1111

arrayTypeToWord8 :: ArrayType -> Word8
arrayTypeToWord8 = fromIntegral . fromEnum

word8ToArrayType :: Word8 -> Get ArrayType
word8ToArrayType w
  |  w >= arrayTypeToWord8 minBound
  && w <= arrayTypeToWord8 maxBound = return $ toEnum $ fromIntegral w
  | otherwise = fail $ "invalid array type specifier: " ++ show w

instance Serialize AllocDirective where
  put ad = case ad of
    AllocVar -> do
      put allocVarMask
    AllocArr at w -> do
      let specifier = arrayTypeToWord8 at
      put specifier
      putWord64be w

  get = do
    specifier <- getWord8
    if | specifier == allocVarMask -> return AllocVar
       | otherwise -> do
           at   <- word8ToArrayType specifier
           size <- getWord64be
           return $ AllocArr at size

data InitDirective
  = InitVar           Word64
  | InitArr ArrayType [Word64]
  deriving Show

initVarMask :: Word8
initVarMask = 0b1111_1111

instance Serialize InitDirective where
  put ad = case ad of
    InitVar w -> do
      put initVarMask
      putWord64be w

    InitArr at ws -> do
      let specifier = arrayTypeToWord8 at
      put specifier
      case at of
        A8 ->  putList putWord8    ws
        A16 -> putList putWord16be ws
        A32 -> putList putWord32be ws
        A64 -> putList putWord64be ws
    where
      putList f = putListOf (f . fromIntegral)

  get = do
    specifier <- getWord8
    if | specifier == initVarMask -> do
           w <- getWord64be
           return $ InitVar w
       | otherwise -> do
           at     <- word8ToArrayType specifier
           case at of
             A8  -> getList at getWord8
             A16 -> getList at getWord16be
             A32 -> getList at getWord32be
             A64 -> getList at getWord64be
    where
      getList at f = do
        ws <- getListOf f
        return $ InitArr at $ map fromIntegral ws

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
