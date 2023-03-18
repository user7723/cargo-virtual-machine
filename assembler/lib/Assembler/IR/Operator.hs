module Assembler.IR.Operator where

import Assembler.IR.InstructionSet
import Assembler.IR.QLabel

import Data.Word
import Data.Bits
import Data.Serialize
import qualified Data.Text as T

import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)

data Operator
  = Nullary Inst
  | Unary   Inst Operand
  deriving Show

data Operand
  = OperandNumber  Word64
  | OperandAddress Address
  deriving Show

data Address
  = Symbolic QLabel
  | Numeric  SegType Word64
  deriving Show

--              speicifie|inst     |operand
--              ari seg  |         |
-- nul i     -- 000 11111|iiii iiii|
-- una i w   -- 001 11111|iiii iiii|wwww wwww  wwww wwww ...
-- una i s a -- 001 00000|iiii iiii|wwww wwww  wwww wwww ...

nonSegMask, nullaryMask, unaryMask, arityMask, segMask :: Word8
nullaryMask = 0b000_00000
unaryMask   = 0b001_00000
nonSegMask  = 0b000_11111

arityMask   = 0b111_00000
segMask     = 0b000_11111

instance Serialize Operator where
  put o = case o of
    Nullary i -> do
      let specifier = nullaryMask .|. nonSegMask
      putWord8 specifier
      put i
    Unary i op ->
      case op of
        OperandNumber w -> do
          let specifier = unaryMask .|. nonSegMask
          putWord8 specifier   -- specifier
          put i                -- inst
          putWord64be w        -- number operand
        OperandAddress a -> case a of
          Numeric s w -> do
            let specifier = unaryMask .|. segTypeToWord8 s
            putWord8 specifier -- specifier
            put i              -- inst
            putWord64be w      -- numeric address operand
          Symbolic ql -> error
                       $ "unresolved symbolic reference: "
                      ++ show ql

  get = do
    specifier <- getWord8
    inst      <- get
    let arity = shiftR (specifier .&. arityMask) 5
        seg   = specifier .&. segMask
    case arity of
      0 -> if | seg == nonSegMask ->
                  return $ Nullary inst
              | otherwise ->
                  fail $ "illegal segment specifier in nullary operator: "
                      ++ showBinary seg
      1 -> if | seg == nonSegMask -> do
                  w <- getWord64be
                  return $ Unary inst $ OperandNumber w
              | otherwise -> do
                  st <- word8ToSegType seg
                  ad <- getWord64be
                  return $ Unary inst $ OperandAddress $ Numeric st ad
      _ -> fail $ "invalid arity specifier: " ++ showBinary arity

showBinary :: Integral a => a -> String
showBinary x = showIntAtBase 2 intToDigit (fromIntegral x) ""
