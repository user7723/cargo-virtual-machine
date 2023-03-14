{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Dependencies where

import Module
import Error
import Parser

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
    decide f = doesFileExist f >>= \case
        True -> return $ Just f
        _    -> return $ Nothing

accumulateProgramCode
  :: ModuleName     -- target
  -> Maybe FilePath -- where to search target module '.' is default
  -> Set FilePath   -- where to search for the dependencies '.' is assumed on empty set
  -> ExceptT Error IO ProgramGraph
-- -> ExceptT Error IO (ModuleName :-> FilePath)
accumulateProgramCode target mtdir depsRoots = do
  depsTbl <- genDependencyTable target mtdir depsRoots
  fmap (fold . M.elems) $ mapM
    ( (programGraph <$>)
    . (parseModuleFromFile :: FilePath -> ExceptT Error IO Module)
    )
    (depsTbl :: ModuleName :-> FilePath)

parseModuleFromFile :: FilePath -> ExceptT Error IO Module
parseModuleFromFile fp = do
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
      excl <- liftIO $ readIORef r
      _    <- liftIO $ modifyIORef r (S.union imps)
      let imps' = imps S.\\ excl
      S.foldr
        (\dep acc -> M.union <$> aux r dirs dep <*> acc)
        (pure $ M.singleton m path)
        imps'
