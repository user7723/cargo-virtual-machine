{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Graph where

import Data.Map (Map)
import qualified Data.Map as M

import Data.Set (Set)
import qualified Data.Set as S

import Data.Text (Text)
import qualified Data.Text as T

import Data.Word
import Data.Foldable (foldl')

import Control.Monad.ST
import Data.STRef

type Label   = Text
type Address = Word64
type (:->)   = Map

data Operation = Operation
  deriving Show

data InitDir   = InitDir
  deriving Show

data AllocDir  = AllocDir
  deriving Show

main :: Node
main = Node
  "main"
  (TextCode $ replicate 5 Operation)
  (S.fromList [f, g, h, bssA, bssB, dataA, dataB])

f :: Node
f = Node
  "f"
  (TextCode $ replicate 2 Operation)
  S.empty

g :: Node
g = Node
  "f"
  (TextCode $ replicate 1 Operation)
  (S.fromList [f, dataA, dataB, main, g, h])

h :: Node
h = Node
  "h"
  (TextCode $ replicate 1 Operation)
  (S.fromList [g, f, bssA, dataA, bssB, h, g, main])

dataA :: Node
dataA = Node
  "dataA"
  (DataCode $ InitDir)
  (S.empty)

dataB :: Node
dataB = Node
  "dataB"
  (DataCode $ InitDir)
  (S.empty)

bssA :: Node
bssA = Node
  "bssA"
  (BssCode $ AllocDir)
  (S.empty)

bssB :: Node
bssB = Node
  "bssB"
  (BssCode $ AllocDir)
  (S.empty)

data Node = Node Label Code (Set Node)
  deriving Show

instance Eq Node where
  Node a _ _ == Node b _ _ = a == b

instance Ord Node where
  Node a _ _ `compare` Node b _ _ = a `compare` b

data Code
  = BssCode  AllocDir
  | DataCode InitDir
  | TextCode [Operation]
  deriving Show

data SectionText = ST (Label :-> (Address, [Operation])) Address
  deriving Show

data SectionData = SD (Label :-> (Address, InitDir)) Address
  deriving Show

data SectionBss  = SB (Label :-> (Address, AllocDir)) Address
  deriving Show

data Sections = Sections
  { sectionText :: SectionText
  , sectionData :: SectionData
  , sectionBss  :: SectionBss
  } deriving Show

insertSection :: Label -> Code -> Sections -> Sections
insertSection l c (Sections t d b) = case c of
  BssCode a    -> Sections t d (insertSB l a b)
  DataCode i   -> Sections t (insertSD l i d) b
  TextCode ops -> Sections (insertST l ops t) d b

insertST :: Label -> [Operation] -> SectionText -> SectionText
insertST l ops (ST m la)
  = let la' = fromIntegral (length ops) + la
    in ST (M.insert l (la, ops) m) la'

insertSD :: Label -> InitDir -> SectionData -> SectionData
insertSD l i (SD m la) = SD (M.insert l (la, i) m) (la+1)

insertSB :: Label -> AllocDir -> SectionBss -> SectionBss
insertSB l a (SB m la) = SB (M.insert l (la, a) m) (la+1)

joinST :: SectionText -> SectionText -> SectionText
joinST (ST l base) (ST r offset)
  = ST (M.foldrWithKey f l r) (base+offset)
  where
    f k (a,is) acc = M.insert k (a+base, is) acc

joinSD :: SectionData -> SectionData -> SectionData
joinSD (SD l base) (SD r offset)
  = SD (M.foldrWithKey f l r) (base+offset)
  where
    f k (a,is) acc = M.insert k (a+base, is) acc

joinSB :: SectionBss -> SectionBss -> SectionBss
joinSB (SB l base) (SB r offset)
  = SB (M.foldrWithKey f l r) (base+offset)
  where
    f k (a,is) acc = M.insert k (a+base, is) acc

joinSections :: Sections -> Sections -> Sections
joinSections (Sections st sd sb) (Sections st' sd' sb')
  = Sections (st `joinST` st')
             (sd `joinSD` sd')
             (sb `joinSB` sb')

emptyText :: SectionText
emptyText = ST M.empty 0

emptyData :: SectionData
emptyData = SD M.empty 0

emptyBss :: SectionBss
emptyBss = SB M.empty 0

emptySection :: Sections
emptySection = Sections emptyText emptyData emptyBss

runGraph :: Node -> Sections
runGraph n = runST $ do
  e <- newSTRef (S.singleton n)
  aux e n
  where
    aux e (Node l c ds) = do
      excl <- readSTRef e
      let
        s = insertSection l c emptySection
        ds' = ds S.\\ excl
        excl' = ds `S.union` excl
      writeSTRef e excl'
      foldl' (\z d -> joinSections <$> z <*> aux e d)
                (pure s)
                (S.toList ds')
