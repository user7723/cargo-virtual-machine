{-# LANGUAGE OverloadedStrings #-}

module Module
  ( Module(..)
  , Header(..)
  , ExportEntry(..)
  , ImportEntry(..)
  , Name
  , Type(..)
  , FunctionalType(..)
  , ConstantType(..)
  , Body(..)
  , AllocDirective(..)
  , AllocVar(..)
  , AllocArray(..)
  , ArrayType(..)
  , InitDirective(..)
  , InitDirectiveVar(..)
  , InitIntegral(..)
  , InitFloating(..)
  , InitDirectiveArray(..)
  , FunctionDef(..)
  , FunctionSignature(..)
  , Parameter(..)
  , Constant(..)
  , FunctionBody
  , LabeledOperator(..)
  , StaticMemoryInst(..)
  , Inst(..)
  , Operator(..)
  , Operand(..)
  , typeToText
  , instToText
  ) where

import Data.Char (toLower)
import Data.Word (Word64)
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
  , exportType   :: Type
  } deriving Show

data ImportEntry = ImportEntry
  { importModuleName  :: Name
  , importModuleAlias :: Maybe Name
  , importSymbols     :: [Name]
  } deriving Show

type Name = Text

data Type
  = FunctTy FunctionalType
  | ConstTy ConstantType
  deriving Show

data FunctionalType = FunctionalType
  { functionInput  :: [ConstantType]
  , functionOutput :: [ConstantType]
  } deriving Show

data ConstantType
  = Unit
  | I64
  | F64
  deriving (Show, Enum, Bounded)

data Body = Body
  { sectionBss  :: [AllocDirective]
  , sectionData :: [InitDirective]
  , sectionText :: [FunctionDef]
  } deriving Show

data AllocDirective
  = AllocDirectiveV AllocVar
  | AllocDirectiveA AllocArray
  deriving Show

data AllocVar = AllocVar
  { allocVarType :: ConstantType
  , allocVarName :: Name
  } deriving Show

data AllocArray = AllocArray
  { allocArrType :: ArrayType
  , allocArrName :: Name
  , allocArrSize :: Word64
  } deriving Show

data ArrayType = A8 | A16 | A32 | A64
  deriving Show

data InitDirective
  = InitDirectiveV InitDirectiveVar
  | InitDirectiveA InitDirectiveArray
  deriving Show

data InitDirectiveVar
  = InitI InitIntegral
  | InitF InitFloating
  deriving Show

data InitIntegral = InitIntegral
   { initIntVarType  :: ConstantType
   , initIntVarName  :: Name
   , initIntVarValue :: Word64
   } deriving Show

data InitFloating = InitFloating
   { initFloatVarType  :: ConstantType
   , initFloatVarName  :: Name
   , initFloatVarValue :: Double
   } deriving Show

data InitDirectiveArray = InitDirectiveArray
  { initArrayType :: ArrayType
  , initArrayName :: Name
  , initArrayValue :: [Word64]
  } deriving Show

data FunctionDef = FunctionDef
  { functionSignature :: FunctionSignature
  , functionBody      :: FunctionBody
  } deriving Show

data FunctionSignature = FunctionSignature
  { functionName   :: Name
  , functionType   :: FunctionalType
  , functionParams :: [Parameter]
  } deriving Show

data Parameter = Parameter
  { parameterName :: Name
  , parameterType :: ConstantType
  } deriving Show

data Constant
  = I64Const Word64
  | F64Const Double
  deriving Show

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
data StaticMemoryInst
  = Alloc_i64 -- unary
  | Alloc_a8  -- binary
  | Init_i64  -- binary
  | Init_a8   -- binary
  deriving Show

-- It's probably not obligatory to specify all the local declarations
-- at the top level of a function before any of the instructions, because
-- none of them are going to interfere with the call stack, except for
-- `call` and `ret`, but they will always recover the previous stack state
-- and state of the VM registers (ip, stack_frame_p, call_stack_p)

data Inst
  = Nop            -- nullary : () -> ()
  | Push_i64       -- nullary : ∀t.() -> (t)
  | Cmp_i64        -- nullary : (i64, i64) -> (i64)
  | Jmp_if         -- unary   : (i64) -> ()
  | Jeq            -- unary   : (i64) -> ()
  | Jle            -- unary   : (i64) -> ()
  | Jmp            -- unary   : () -> ()
  -- call instruction is going to accept amount of arguments that
  -- are must be moved from the operand stack to the call stack and
  -- an address where it is supposed to jump at
  | Call           -- binary  : () -> ()
  | Ret            -- nullary : () -> ()
  | Mul_i64        -- nullary : (i64, i64) -> (i64)
  | Div_i64        -- nullary : (i64, i64) -> (i64)
  | Add_i64        -- nullary : (i64, i64) -> (i64)
  | Sub_i64        -- nullary : (i64, i64) -> (i64)

-- Local Scope Instructions
  -- decl - occupies place at the call stack for a local variable
  | Local_decl_i64 -- unary
  -- bind - either declares and initializes a variable or binds declared one
  | Local_bind_i64 -- unary
  -- read - reads a value of local variable
  | Local_read_i64 -- unary

-- VM Memory Instructions
  | Load_i64       -- unary : () -> (i64)
  | Load_a64_i64   -- unary : (offset:i64) -> (i64)
  | Store_i64      -- unary : (val:i64) -> ()
  | Store_a64_i64  -- unary : (val:i64, offset:i64) -> ()
  deriving (Show, Enum, Bounded)

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

data Operand
  = OperandNumber Word64
  | OperandName   Name
  deriving Show

instToText :: Inst -> Text
instToText i =
  case show i of
    (c:cs) -> pack $ toLower c : cs
    _      -> error "unreachable"

typeToText :: ConstantType -> Text
typeToText Unit = "()"
typeToText t =
  case show t of
    (c:cs) -> pack $ toLower c : cs
    _      -> error "unreachable"
