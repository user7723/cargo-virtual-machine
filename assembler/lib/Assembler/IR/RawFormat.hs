module Assembler.IR.RawFormat where

import Assembler.IR.Operator
import Assembler.IR.StaticMemory
import Assembler.IR.QLabel

import Data.Word

data ProgramText = ProgramText
  { programTextSeg :: [Operator]
  , programDataSeg :: [InitDirective]
  , programBssSeg  :: [AllocDirective]
  , textSegSymbols :: [(QLabel, Word64)]
  } deriving Show
