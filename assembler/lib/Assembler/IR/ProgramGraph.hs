module Assembler.IR.ProgramGraph where

import Assembler.IR.Aliases
import Assembler.IR.StaticMemory
import Assembler.IR.FunctionDef
import Assembler.IR.QLabel
import Data.Set (Set)

type ProgramGraph = QLabel :-> Node

data Node = Node Code (Set QLabel)
  deriving Show

data Code
  = BssCode  AllocDirective
  | DataCode InitDirective
  | TextCode FunctionDef
  deriving Show

