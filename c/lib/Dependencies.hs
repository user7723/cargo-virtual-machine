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

sourceCodeSuffix :: FilePath
sourceCodeSuffix = "casm"

findModulePath :: ModuleName -> Set FilePath -> ExceptT Error IO FilePath
findModulePath mn roots = do
  let dirs = T.unpack $ T.intercalate "/" mn
      possible = map (</> (dirs <.> sourceCodeSuffix))
               $ S.toList roots

  liftIO (catMaybes <$> mapM decide possible) >>= \case
    (path:_) -> return path
    _        -> throwE $ ModuleWasNotFound mn roots
  where
    -- FIXME: IOExeption -> EitherT Error
    decide f = doesFileExist f >>= \case
        True -> return $ Just f
        _    -> return $ Nothing

accumulateProgramCode
  :: ModuleName :-> FilePath
  -> ExceptT Error IO ProgramGraph
accumulateProgramCode depsTbl = do
  fmap (fold . M.elems) $ mapM
    ( (programGraph <$>)
    . parseModuleFromFile
    )
    depsTbl

extractDependencyCode
  :: QLabel                -- Main
  -> ProgramGraph          -- complete program graph
  -> ExceptT Error IO Segs -- nodes of the program that are reachable from Main
extractDependencyCode start pg
  = mapExceptT stToIO $ runGraph start pg


readProgramCode
  :: ModuleName
  -> Maybe FilePath -- where to search target module '.' is default
  -> Set FilePath   -- where to search for the dependencies '.' is assumed on empty set
  -> ExceptT Error IO Segs
readProgramCode t tfp depsDirs = do
  depsTbl <- genDependencyTable t tfp depsDirs
  tFile   <- maybe
    ( throwE
    $ ModuleWasNotFound
      t
      (S.insert (fromMaybe "." tfp) depsDirs)
    )
    pure
    (M.lookup t depsTbl)
  mstart  <- parseMainFromFile tFile
  case mstart of
    Nothing    -> throwE $ NoEntryPointWasGiven t tFile
    Just start -> do
      accPG   <- accumulateProgramCode depsTbl
      liftIO $ pPrint accPG
      extractDependencyCode start accPG


parseModuleFromFile :: FilePath -> ExceptT Error IO Module
parseModuleFromFile fp = do
  -- FIXME: IOExeption -> EitherT Error
  sc <- liftIO $ T.readFile fp
  parseExcept parseModule fp sc

genDependencyTable
  :: ModuleName
  -> Maybe FilePath -- where to search target module '.' is default
  -> Set FilePath   -- where to search for the dependencies '.' is assumed on empty set
  -> ExceptT Error IO (ModuleName :-> FilePath)
genDependencyTable mn mtdir roots = do
  rex  <- liftIO $ newIORef (S.singleton mn)
  tdir <- liftIO $ maybe getCurrentDirectory pure mtdir
  aux rex (S.insert tdir roots) mn
  where
    aux r dirs m = do
      path <- findModulePath m dirs
      imps <- parseImportsFromFile m path

      -- FIXME: IOExeption -> EitherT Error
      excl <- liftIO $ readIORef r
      -- FIXME: IOExeption -> EitherT Error
      _    <- liftIO $ modifyIORef r (S.union imps)
      let imps' = imps S.\\ excl
      S.foldr
        (\dep acc -> M.union <$> aux r dirs dep <*> acc)
        (pure $ M.singleton m path)
        imps'
