{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeOperators #-}

module Module where

import Data.Char (toLower)
import Data.Word
import Data.Text (Text)
import Data.Text (pack)

import Data.Map (Map)
import qualified Data.Map as M

import Data.Set (Set)
import qualified Data.Set as S

import Data.Text (Text)
import qualified Data.Text as T

import ParserTypes

type (:->)      = Map
type Name       = Text
type Label      = Text
type ModuleName = [Text]

data QLabel = QLabel
  { labelQualifier :: ModuleName
  , labelSegType   :: SegType
  , labelName      :: Label
  , labelRelative  :: Maybe Label
  } deriving (Show, Eq, Ord)

data Node = Node Code (Set QLabel)
  deriving Show

type ProgramGraph = QLabel :-> Node
type ModulesGraph = QLabel :-> Node

data Module = Module
  { moduleName   :: ModuleName
  , entryPoint   :: Maybe QLabel
  , programGraph :: QLabel :-> Node
  -- , dependencies :: Set ModuleName
  } deriving Show

data Code
  = BssCode  AllocDirective
  | DataCode InitDirective
  | TextCode FunctionDef
  deriving Show

-- Should be subject of abstraction in some kind of header file,
-- so the definitions of this enumeration will match between the
-- assembler and the virtual machine without necessity of constant
-- manual checks. Maybe it's simpler to generate c-header file
-- after each assembler recompilation instead of reading it in
-- using FFI tools
data SegType
  = Data
  | Bss
  | Text
  | Local
--  | Absolute
  deriving (Show, Eq, Ord, Enum, Bounded)

data Access = LocalScope | Readable | Executable
  deriving Show

numberToSegType :: Access -> Word64 -> Maybe SegType
numberToSegType a n
  | check n   = Just $ toEnum $ fromIntegral n
  | otherwise = Nothing
  where
    minS = fromIntegral $ fromEnum (minBound :: SegType)
    maxS = fromIntegral $ fromEnum (maxBound :: SegType)
    check x = all ($ x) [(>= minS), (<= maxS), accessMatch]
    accessMatch = case a of
      Readable   -> (/= textIdx)
      Executable -> (== textIdx)
    textIdx = fromIntegral $ fromEnum Text

data AllocDirective
  = AllocVar
  | AllocArr ArrayType Word64
  deriving Show

data InitDirective
  = InitVar           Word64
  | InitArr ArrayType [Word64]
  deriving Show

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


txtLower :: Show a => a -> Text
txtLower x = pack $ map toLower (show x)

data ArrayType
  = A8
  | A16
  | A32
  | A64
  deriving (Enum, Bounded)

instance Show ArrayType where
  show A8  = "8"
  show A16 = "16"
  show A32 = "32"
  show A64 = "64"

arrayTypeToText :: ArrayType -> Text
arrayTypeToText = txtLower

elemCapacity :: Num a => ArrayType -> a
elemCapacity A8  = fromIntegral (maxBound :: Word8)
elemCapacity A16 = fromIntegral (maxBound :: Word16)
elemCapacity A32 = fromIntegral (maxBound :: Word32)
elemCapacity A64 = fromIntegral (maxBound :: Word64)

-- It's probably not obligatory to specify all the local declarations
-- at the top level of a function before any of the instructions, because
-- none of them are going to interfere with the call stack, except for
-- `call` and `ret`, but they will always recover the previous stack state
-- and state of the VM registers (ip, stack_frame_p, call_stack_p)

data Inst
  = Nop            -- nullary : () -> ()
  | Push           -- nullary : ∀t.() -> (t)
  | Drop           -- nullary : ∀t.(t) -> (t)
  | Icmp           -- nullary : (i64, i64) -> (i64)
  | Jmp_if         -- unary   : (i64) -> ()
  | Jeq            -- unary   : (i64) -> ()
  | Jle            -- unary   : (i64) -> ()
  | Jmp            -- unary   : () -> ()
  -- call instruction is going to accept amount of arguments that
  -- are must be moved from the operand stack to the call stack and
  -- an address where it is supposed to jump at
  | Call           -- unary   : () -> ()
  | Ret            -- nullary : () -> ()
  | Imul           -- nullary : (i64, i64) -> (i64)
  | Idiv           -- nullary : (i64, i64) -> (i64)
  | Iadd           -- nullary : (i64, i64) -> (i64)
  | Isub           -- nullary : (i64, i64) -> (i64)

-- Local Scope Instructions
  -- decl - occupies place at the call stack for a local variable
  | Decl          -- nullary

-- VM Memory Instructions
  | Read          -- nullary : (addr:i64) -> (i64)
  | Read_a64      -- nullary : (addr:i64, off:i64) -> (i64)
  | Write         -- nullary : (addr:i64, val:i64) -> ()
  | Write_a64     -- nullary : (addr:i64, off:i64, val:i64) -> ()
  deriving (Show, Enum, Bounded)

instToText :: Inst -> Text
instToText = txtLower

-- Arity of an instruction represents, not the amount of arguments that
-- it will take from the operand stack, but the amount of arguments that
-- should be considered as a part of instruction itself e.g.
-- jmp <addr>             - unary
-- store_i64 <var_name>   - unary
-- call <argc> <function> - binary
data Operator
  = Nullary Inst                 -- FunctionalType
  | Unary   Inst Operand         -- FunctionalType
  deriving Show

data Operand
  = OperandNumber  Word64
  | OperandAddress Address
  deriving Show

data Address
  = Symbolic QLabel
  | Numeric  SegType Word64
  deriving Show

{-
-- t segment is always asumed
  call [t:]<function name>
  call [t:]seq.fibonacci

-- t segment is assumed and the offset is computed at assembly time
-- as sum of function address and its local label address
  jmp  [t:<function name>.]<rel label>

  l:n ~> push 100...<48 address bits>
  read

  b:x ~> push 011...<48 address bits>
  42
  write

-- t is dissallowed
-- segment prefix is requred in order to avoid ambiguity/shadowing problem
  push (l|b|d):<label name>
-}

