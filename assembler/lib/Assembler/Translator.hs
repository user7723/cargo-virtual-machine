module Assembler.Translator where

import Assembler.IR.Aliases
import Assembler.IR.RawFormat

import Assembler.Resolver.SymbolicRefs
import Assembler.Resolver.ProgramCodeDeps
import Assembler.Error

import Control.Monad.Trans.Except

import Data.Set (Set)
import Data.Word

data Magic = Magic
  deriving Show

data ByteCode = ByteCode
  { magicBytes   :: Magic
  , programStart :: Word64
  , programText  :: ProgramText
  } deriving Show

readProgramText
  :: ModuleName
  -> Set FilePath   -- where to search for the dependencies '.' is assumed on empty set
  -> ExceptT Error IO ProgramText
readProgramText mn sfp = do
  segs <- extractDependencyCode mn sfp
  pure $ resolveSymbolicRefs segs
