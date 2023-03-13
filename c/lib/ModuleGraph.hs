module ModulesGraph where

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Map (Map)
import qualified Data.Map as M

import Data.Set (Set)
import qualified Data.Set as S

import Data.Text (Text)
import qualified Data.Text as T

import Data.Word
import Data.Foldable (foldl')

import Data.IORef

import Control.Monad.Trans.Except
import Control.Monad.IO.Class

type Address = Word64
type (:->)   = Map

type Label      = Text
type ModuleName = Text
data QLabel     = QLabel ModuleName Label
  deriving (Show, Eq, Ord)

data Node = Node Code (Set QLabel)
  deriving Show

data AllocDirective = AllocDirective
  deriving Show

data InitDirective = InitDirective
  deriving Show

data FunctionDef = FunctionDef
  deriving Show


data Code
  = BssCode  AllocDirective
  | DataCode InitDirective
  | TextCode FunctionDef
  deriving Show

type ProgramGraph = QLabel :-> Node
type ModulesGraph = QLabel :-> Node

data Module = Module
  { entryPoint   :: QLabel
  , moduleName   :: ModuleName
  , programGraph :: QLabel :-> Node
  , dependencies :: Set ModuleName
  } deriving Show

data Error = Error

parseModuleFromFile :: FilePath -> ExceptT Error IO Module
parseModuleFromFile = undefined

getModuleName :: FilePath -> IO (Maybe ModuleName)
getModuleName = undefined

joinGraphs :: ModulesGraph -> ModulesGraph -> ModulesGraph
joinGraphs = M.union

type Target = ModuleName

-- -t <Module Name> [-d <Target dir>] [-I <Dep dir> [-I <Dep dir> ...]]
fetchDependencies :: Target -> Map ModuleName FilePath
fetchDependencies = undefined


collectModulesGraph
  :: Map ModuleName FilePath
  -> Target
  -> ExceptT Error IO ModulesGraph
collectModulesGraph ps mn = do
  rex <- liftIO $ newIORef S.empty
  aux rex mn
  where
    aux rex mname = do
      mo <- maybe
        (throwE Error)
        parseModuleFromFile
        (M.lookup mname ps)
      ex <- liftIO $ readIORef rex
      let ex'  = S.insert (moduleName mo) ex
          ds   = dependencies mo
          ex'' = S.union ex' ds
          ds'  = ds S.\\ ex'
      liftIO $ writeIORef rex ex''
      foldl' (\z d -> joinGraphs <$> z <*> aux rex d)
                (pure $ programGraph mo)
                (S.toList ds')
