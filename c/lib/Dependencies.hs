{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Dependencies where

import Text.Show.Pretty

import Module
import Error
import Parser
import ProgramGraphTraversal

import System.FilePath
import System.Directory
import Data.Maybe
import Data.Foldable

import Data.Set (Set)
import qualified Data.Set as S

import Data.Map (Map)
import qualified Data.Map as M

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.IO
import Data.IORef

import Control.Monad.Trans.Except
import Control.Monad.IO.Class

import Control.Monad.ST

import Control.Monad

sourceCodeSuffix :: FilePath
sourceCodeSuffix = "casm"

type Directory        = FilePath
type SourceCodePath   = FilePath
type TargetFilePath   = FilePath
type TargetModuleName = ModuleName
type CompleteProgramGraph = ProgramGraph

extractDependencyCode
  :: TargetModuleName
  -> Set Directory
  -> ExceptT Error IO Segs
extractDependencyCode tmn dirs = do
  (tpath, deps) <- fetchSourceCodePaths tmn dirs
  completePG    <- accumulateProgramCode deps
  parseMainFromFile tpath >>= \case
    Just start -> mapExceptT stToIO $ runGraph start completePG
    Nothing    -> throwE $ NoEntryPointWasGiven tmn tpath

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
