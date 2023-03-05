{-# LANGUAGE OverloadedStrings #-}

module Module
  ( Module(..)
  , Header(..)
  , ExportEntry(..)
  , ImportEntry(..)
  , Name
  , Body(..)
  , AllocDirective(..)
  , InitDirective(..)
  , ArrayType(..)
  , FunctionDef(..)
  , FunctionSignature(..)
  , FunctionBody
  , LabeledOperator(..)
  , Inst(..)
  , Operator(..)
  , Operand(..)
  , QualifiedName(..)
  , txtLower
  , arrayTypeToText
  , elemCapacity
  , instToText
  ) where

import Data.Char (toLower)
import Data.Word
import Data.Text (Text)
import Data.Text (pack)

data Module = Module
  -- Module name
  -- Export list
  -- Import list
  { moduleHeader :: Header
  -- .bss, .data and .text sections
  , moduleBody   :: Body
  -- Not shure about the Footer, it might be kind of like the
  -- system's break that indicates the end of the program
  -- and begining of the heap memory region
--, moduleFooter :: Footer 
  } deriving Show
data Header = Header
  { moduleName    :: Name
  , moduleMain    :: Maybe Name
  , moduleExports :: [ExportEntry]
  , moduleImports :: [ImportEntry]
  } deriving Show

data ExportEntry = ExportEntry
  { exportSymbol :: Name
  , exportType   :: [Name]
  } deriving Show

data ImportEntry = ImportEntry
  { importModuleName  :: Name
  , importModuleAlias :: Maybe Name
  , importSymbols     :: [Name]
  } deriving Show

type Name = Text

data Body = Body
  { sectionBss  :: [AllocDirective]
  , sectionData :: [InitDirective]
  , sectionText :: [FunctionDef]
  } deriving Show

data AllocDirective
  = AllocVar           Name
  | AllocArr ArrayType Name Word64
  deriving Show

data InitDirective
  = InitVar           Name Word64
  | InitArr ArrayType Name [Word64]
  deriving Show

data FunctionDef = FunctionDef
  { functionSignature :: FunctionSignature
  , functionBody      :: FunctionBody
  } deriving Show

data FunctionSignature = FunctionSignature
  { functionName   :: Name
  , functionParams :: [Name]
  } deriving Show

type FunctionBody = [LabeledOperator]

data LabeledOperator = LabeledOperator
  { operatorLabels  :: [Name]
  , labeledOperator :: Operator
  } deriving Show

-- push 1 : () -> i64
-- drop : i64 -> ()
-- add : (i64, i64) -> i64
-- mul : (i64, i64) -> i64
-- div : (i64, i64) -> i64
-- sub : (i64, i64) -> i64

-- jmp label : () -> ()

-- These are only going to be allowed inside
-- of a static memory specification sections (.bss/.data)
-- So, they might not occupy the same encoding space
-- for usual instructions such as add, mul etc.

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
  | Ipush          -- nullary : ∀t.() -> (t)
  | Icmp           -- nullary : (i64, i64) -> (i64)
  | Jmp_if         -- unary   : (i64) -> ()
  | Jeq            -- unary   : (i64) -> ()
  | Jle            -- unary   : (i64) -> ()
  | Jmp            -- unary   : () -> ()
  -- call instruction is going to accept amount of arguments that
  -- are must be moved from the operand stack to the call stack and
  -- an address where it is supposed to jump at
  | Call           -- binary  : () -> ()
  | Ret            -- nullary : () -> ()
  | Imul           -- nullary : (i64, i64) -> (i64)
  | Idiv           -- nullary : (i64, i64) -> (i64)
  | Iadd           -- nullary : (i64, i64) -> (i64)
  | Isub           -- nullary : (i64, i64) -> (i64)

-- Local Scope Instructions
  -- decl - occupies place at the call stack for a local variable
  | Idecl          -- unary
  -- bind - either declares and initializes a variable or binds declared one
  | Ibind          -- unary
  -- read - reads a value of local variable
  | Iread          -- unary

-- VM Memory Instructions
  | Iload          -- unary : () -> (i64)
  | Iload_a64      -- unary : (offset:i64) -> (i64)
  | Istore         -- unary : (val:i64) -> ()
  | Istore_a64     -- unary : (val:i64, offset:i64) -> ()
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
  | Binary  Inst Operand Operand -- FunctionalType
  deriving Show

data QualifiedName = QualifiedName
  { qNameSpace :: [Name]
  , qName      :: Name
  } deriving Show

data Operand
  = OperandNumber    Word64
  | OperandName      Name
  | OperandQualified QualifiedName
  deriving Show
