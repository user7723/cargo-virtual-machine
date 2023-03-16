module Translation where

import Dependencies -- readProgramCode
import Module
import SymbolsResolution -- resolveSymbolicRefs
import Error

import Control.Monad.Trans.Except
import Control.Monad.IO.Class

import Data.Set (Set)
import qualified Data.Set as S

readProgramText
  :: ModuleName
  -> Set FilePath   -- where to search for the dependencies '.' is assumed on empty set
  -> ExceptT Error IO ProgramText
readProgramText mn sfp = do
  segs <- extractDependencyCode mn sfp
  pure $ resolveSymbolicRefs segs
