module Assembler.Parser.ProgramGraph where

import Assembler.IR.Aliases
import Assembler.IR.ProgramGraph

import Assembler.Parser.Aliases
import Assembler.Parser.StaticMemory
import Assembler.Parser.FunctionDef

parseProgram :: ModuleName -> Parser ProgramGraph
parseProgram mn = do
  bss <- parseSegTypeBss mn
  dat <- parseSegTypeData mn
  txt <- parseSegTypeText mn
  pure $ bss <> dat <> txt
