module Assembler.Resolver.ProgramCodeDeps where

import Assembler.Error

import Assembler.IR.Aliases
import Assembler.IR.QLabel
import Assembler.IR.StaticMemory
import Assembler.IR.ProgramGraph
import Assembler.IR.FunctionDef

import Assembler.Resolver.Aliases
import Assembler.Resolver.ModuleDeps

import Assembler.Parser.IO

import qualified Data.Map as M

import Data.Set (Set)
import qualified Data.Set as S

import Data.Word
import Data.Foldable (foldl')

import Control.Monad.ST
import Data.STRef

import Control.Monad.Trans.Except
import Control.Monad.Trans.Class

data SegText = SegText (QLabel :-> (Word64, FunctionDef)) Word64
  deriving Show

data SegData = SegData (QLabel :-> (Word64, InitDirective)) Word64
  deriving Show

data SegBss  = SegBss  (QLabel :-> (Word64, AllocDirective)) Word64
  deriving Show

data Segs = Segs
  { sectionText :: SegText
  , sectionData :: SegData
  , sectionBss  :: SegBss
  } deriving Show

insertSeg :: QLabel -> Code -> Segs -> Segs
insertSeg l c (Segs t d b) = case c of
  BssCode a    -> Segs t d (insertSegBss l a b)
  DataCode i   -> Segs t (insertSegData l i d) b
  TextCode ops -> Segs (insertSegText l ops t) d b

insertSegText :: QLabel -> FunctionDef -> SegText -> SegText
insertSegText l fd (SegText m la)
  = let la' = fromIntegral (M.size $ functionInsts fd) + la
    in SegText (M.insert l (la, fd) m) la'

insertSegData :: QLabel -> InitDirective -> SegData -> SegData
insertSegData l i (SegData m la)
  = SegData (M.insert l (la, i) m) (la+1)

insertSegBss :: QLabel -> AllocDirective -> SegBss -> SegBss
insertSegBss l a (SegBss m la)
  = SegBss (M.insert l (la, a) m) (la+1)

joinSegText :: SegText -> SegText -> SegText
joinSegText (SegText l base) (SegText r offset)
  = SegText (M.foldrWithKey f l r) (base+offset)
  where
    f k (a,is) acc = M.insert k (a+base, is) acc

joinSegData :: SegData -> SegData -> SegData
joinSegData (SegData l base) (SegData r offset)
  = SegData (M.foldrWithKey f l r) (base+offset)
  where
    f k (a,is) acc = M.insert k (a+base, is) acc

joinSegBss :: SegBss -> SegBss -> SegBss
joinSegBss (SegBss l base) (SegBss r offset)
  = SegBss (M.foldrWithKey f l r) (base+offset)
  where
    f k (a,is) acc = M.insert k (a+base, is) acc

joinSegs :: Segs -> Segs -> Segs
joinSegs (Segs st sd sb) (Segs st' sd' sb')
  = Segs (st `joinSegText` st')
         (sd `joinSegData` sd')
         (sb `joinSegBss` sb')

emptyText :: SegText
emptyText = SegText M.empty 0

emptyData :: SegData
emptyData = SegData M.empty 0

emptyBss :: SegBss
emptyBss = SegBss M.empty 0

emptySeg :: Segs
emptySeg = Segs emptyText emptyData emptyBss

programLookup
  :: Monad m
  => QLabel
  -> ProgramGraph
  -> ExceptT Error m Node
programLookup ql pg = except $
  case M.lookup ql pg of
    Just n  -> pure n
    Nothing -> Left $ UndefinedReference ql

type EntryPoint = QLabel

runGraph
  :: forall s
   . EntryPoint
  -> ProgramGraph
  -> ExceptT Error (ST s) Segs
runGraph start pg = do
  e <- lift $ newSTRef (S.singleton start)
  aux e start
  where
    aux rex ql = do
      excl <- lift $ readSTRef rex
      node <- programLookup ql pg
      let
        (Node code deps) = node
        seg   = insertSeg ql code emptySeg
        deps' = deps S.\\ excl
        excl' = deps `S.union` excl
      lift $ writeSTRef rex excl'
      foldl' (\z d -> joinSegs <$> z <*> aux rex d)
        (pure seg)
        (S.toList deps')

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
