module Assembler.IR.Module where

import Assembler.IR.Aliases
import Assembler.IR.ProgramGraph
import Assembler.IR.QLabel

data Module = Module
  { moduleName   :: ModuleName
  , entryPoint   :: Maybe QLabel
  , programGraph :: ProgramGraph
  } deriving Show
