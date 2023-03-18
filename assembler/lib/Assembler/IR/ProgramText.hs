module Assembler.IR.ProgramText where

import Assembler.IR.QLabel
import Assembler.IR.StaticMemory
import Assembler.IR.Operator

import Data.Word
import Data.Serialize

data ProgramText = ProgramText
  { programMagic   :: Magic
  , programStart   :: Word64
  , programTextSeg :: [Operator]
  , programDataSeg :: [InitDirective]
  , programBssSeg  :: [AllocDirective]
  , textSegSymbols :: [(QLabel, Word64)]
  } deriving Show

instance Serialize ProgramText where
  put ProgramText{..} = do
    put           programMagic
    putWord64be   programStart
    putListOf put programTextSeg
    putListOf put programDataSeg
    putListOf put programBssSeg
    putListOf (putTwoOf put putWord64be) textSegSymbols

  get = do
    mag <- get
    sta <- getWord64be
    txt <- getListOf get
    dat <- getListOf get
    bss <- getListOf get
    tsy <- getListOf (getTwoOf get getWord64be)
    return $ ProgramText { programMagic   = mag
                         , programStart   = sta
                         , programTextSeg = txt
                         , programDataSeg = dat
                         , programBssSeg  = bss
                         , textSegSymbols = tsy
                         }

data Magic = Magic
  deriving Show

magicBits :: Word64
magicBits = 0xdeadbabe

instance Serialize Magic where
  put _ = putWord64be magicBits
  get = do
    m <- getWord64be
    if | m == magicBits -> return Magic
       | otherwise -> fail "no magic bytes was found"
