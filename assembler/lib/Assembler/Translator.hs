module Assembler.Translator where

import Assembler.IR.Aliases
import Assembler.IR.ProgramText

import Assembler.Resolver.SymbolicRefs
import Assembler.Resolver.ProgramCodeDeps
import Assembler.Error

import Control.Monad.Trans.Except

import Data.Set (Set)
import Data.ByteString (ByteString)

import Data.Serialize

readProgramText
  :: ModuleName
  -> Set FilePath   -- where to search for the dependencies '.' is assumed on empty set
  -> ExceptT Error IO ProgramText
readProgramText mn sfp = do
  (start, segs) <- extractDependencyCode mn sfp
  return $ resolveSymbolicRefs start segs

translate :: ProgramText -> ByteString
translate = encode
