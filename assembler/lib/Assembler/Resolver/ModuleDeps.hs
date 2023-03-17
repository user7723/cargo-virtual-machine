module Assembler.Resolver.ModuleDeps where

import Assembler.Error

import Assembler.IR.Aliases
import Assembler.IR.ProgramGraph
import Assembler.IR.Module

import Assembler.Parser.IO
import Assembler.Parser.Module

import Assembler.Resolver.Aliases

import System.FilePath
import System.Directory
import Data.Foldable

import Data.Set (Set)
import qualified Data.Set as S

import qualified Data.Text as T

import Data.IORef

import Control.Monad.Trans.Except
import Control.Monad.IO.Class

import Control.Monad

sourceCodeSuffix :: FilePath
sourceCodeSuffix = "casm"

fetchSourceCodePaths
  :: TargetModuleName
  -> Set Directory
  -> ExceptT Error IO (TargetFilePath, Set SourceCodePath)
fetchSourceCodePaths mn roots = do
  rex  <- liftIO $ newIORef (S.singleton mn)
  p    <- findModulePath mn roots
  ps   <- aux rex roots mn
  pure (p, ps)
  where
    aux r dirs m = do
      path <- findModulePath m dirs
      imps <- parseImportsFromFile m path
      excl <- liftIO $ readIORef r
      _    <- liftIO $ modifyIORef r (S.union imps)
      let imps' = imps S.\\ excl
      S.foldr
        (\dep acc -> S.union <$> aux r dirs dep <*> acc)
        (pure $ S.singleton path)
        imps'

findModulePath
  :: ModuleName
  -> Set Directory
  -> ExceptT Error IO SourceCodePath
findModulePath mn roots = do
  liftIO (filterM doesFileExist possibleModulePaths) >>= \case
    (path:_) -> return path
    _        -> throwE $ ModuleWasNotFound mn roots
  where
    possibleModulePaths = appendRelativePath <$> S.toList roots
    appendRelativePath = (</> (moduleNameAsPath <.> sourceCodeSuffix))
    moduleNameAsPath = joinPath $ T.unpack <$> mn

accumulateProgramCode
  :: Set SourceCodePath
  -> ExceptT Error IO CompleteProgramGraph
accumulateProgramCode
  = (fold <$>)
  . mapM getProgramGraph
  . S.toList
  where
    getProgramGraph :: SourceCodePath -> ExceptT Error IO ProgramGraph
    getProgramGraph fp = programGraph <$> parseFromFile fp parseModule
