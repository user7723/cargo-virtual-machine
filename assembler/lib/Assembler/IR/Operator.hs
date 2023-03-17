module Assembler.IR.Operator where

import Assembler.IR.InstructionSet
import Assembler.IR.QLabel

import Data.Word

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
