module Assembler.IR.FunctionDef where

import Assembler.IR.Aliases
import Assembler.IR.Operator
import Assembler.IR.QLabel

import Data.Word
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map as M

data FunctionDef = FunctionDef
  { functionName       :: QLabel
  , functionScope      :: Label  :-> Word64
  , functionInsts      :: Word64 :-> Operator
  , functionLabels     :: Label  :-> Word64
  , functionDeps       :: Set QLabel
  , functionUnresolved :: Set Word64
  } deriving Show

findLocalIndex :: FunctionDef -> Label -> Maybe Word64
findLocalIndex fd l = M.lookup l $ functionScope fd

validLocalIndex :: FunctionDef -> Word64 -> Maybe Word64
validLocalIndex fd idx
  | idx < varsCnt = Just idx
  | otherwise     = Nothing
  where
    varsCnt = fromIntegral $ M.size (functionScope fd)

emptyFunctionDef :: QLabel -> FunctionDef
emptyFunctionDef name = FunctionDef
  { functionName   = name
  , functionScope  = mempty
  , functionInsts  = mempty
  , functionLabels = mempty
  , functionDeps   = mempty
  , functionUnresolved = mempty
  }

insertDep :: QLabel -> FunctionDef -> FunctionDef
insertDep ql fd =
  let deps = functionDeps fd
  in fd { functionDeps = S.insert ql deps }

insertLocal :: Label -> Word64 -> FunctionDef -> FunctionDef
insertLocal l i fd =
  let s = functionScope fd
  in fd { functionScope = M.insert l i s }

insertInst :: Word64 -> Operator -> FunctionDef -> FunctionDef
insertInst i op fd =
  let ops = functionInsts fd
  in fd { functionInsts = M.insert i op ops }

insertLabel :: Label -> Word64 -> FunctionDef -> FunctionDef
insertLabel l i fd =
  let ls = functionLabels fd
  in fd { functionLabels = M.insert l i ls }

insertUnresolved :: Word64 -> Operator -> FunctionDef -> FunctionDef
insertUnresolved idx (Unary _ (OperandAddress (Symbolic _))) fd
  = let unres = S.insert idx $ functionUnresolved fd
    in fd { functionUnresolved = unres }
insertUnresolved _ _ fd = fd
