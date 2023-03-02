module Module where

import Data.Word (Word64, Word8)
import Data.ByteString (ByteString)

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
  , moduleMain    :: Maybe Symbol
  , moduleExports :: [ExportEntry]
  , moduleImports :: [ImportEntry]
  } deriving Show

data ExportEntry = ExportEntry
  { exportSymbol :: Symbol
  , exportType   :: Type
  } deriving Show

data ImportEntry = ImportEntry
  { importModuleName    :: Name
  , importModuleAliases :: [Symbol]
  , importSymbols       :: [Symbol]
  } deriving Show

type Name = ByteString

data Symbol = Symbol
  { symbolName :: Name
  , symbolAddr :: Maybe Word64
  } deriving Show

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
  deriving Show

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
  , allocVarName :: Symbol
  } deriving Show

data AllocArray = AllocArray
  { allocArrType :: ArrayType
  , allocArrName :: Symbol
  , allocArrSize :: Word64
  } deriving Show

data ArrayType = A8 | A16 | A32 | A64
  deriving Show

data InitDirective = InitDirective
  { initSymbol :: Symbol
  , initArray  :: [Word8]
  } deriving Show

data FunctionDef = FunctionDef
  { functionSignature :: FunctionSignature
  , functionBody      :: FunctionBody
  } deriving Show

data FunctionSignature = FunctionSignature
  { functionName   :: Symbol
  , functionType   :: FunctionalType
  , functionParams :: [Parameter]
  } deriving Show

data Parameter = Parameter
  { parameterName :: Symbol
  , parameterType :: ConstantType
  } deriving Show

data Variable = Variable
  { variableName  :: Symbol
  , variableType  :: ConstantType
  , variableValue :: Constant
  } deriving Show

data Constant
  = I64Const Word64
  | F64Const Double
  deriving Show

data FunctionBody = FunctionBody
  { functionInsts  :: [Inst]
  , functionScope  :: [Variable]
  , functionLabels :: [Symbol]
  } deriving Show


-- push 1 : () -> i64
-- drop : i64 -> ()
-- add : (i64, i64) -> i64
-- mul : (i64, i64) -> i64
-- div : (i64, i64) -> i64
-- sub : (i64, i64) -> i64

-- jmp label : () -> ()

data Inst
  = Inst_nop
  | Inst_alloc_i64
  | Inst_init_i64
  | Inst_get_var
  | Inst_push
  | Inst_cmp_i64
  | Inst_jle
  | Inst_call
  | Inst_ret
  | Inst_mul_i64
  | Inst_dec_i64
  deriving (Show, Enum)
