module Assembler.IR.Text where

-- import Assembler.IR.Aliases
-- import Assembler.IR.FunctionDef
-- import Assembler.IR.Module
-- import Assembler.IR.Operator
-- import Assembler.IR.ProgramGraph
-- import Assembler.IR.StaticMemory
import Assembler.IR.InstructionSet
-- import Assembler.IR.QLabel

import Data.Char
import Data.Text (Text)
import qualified Data.Text as T

showTextLower :: Show a => a -> Text
showTextLower x = T.pack $ map toLower (show x)

instToText :: Inst -> Text
instToText = showTextLower
