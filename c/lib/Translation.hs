module Translation where

import Dependencies -- readProgramCode
import Module
import SymbolsResolution -- resolveSymbolicRefs
import Error

import Control.Monad.Trans.Except
import Control.Monad.IO.Class

import Data.Set (Set)

readProgramText
  :: ModuleName
  -> Maybe FilePath -- where to search target module '.' is default
  -> Set FilePath   -- where to search for the dependencies '.' is assumed on empty set
  -> ExceptT Error IO ProgramText
readProgramText mn mfp sfp = do
  segs <- readProgramCode mn mfp sfp
  pure $ resolveSymbolicRefs segs
