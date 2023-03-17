module Assembler.IR.Text where

import Assembler.IR.Operator
import Assembler.IR.StaticMemory
import Assembler.IR.InstructionSet
import Assembler.IR.QLabel

import Assembler.IR.RawFormat

import Data.Word
import Data.Text (Text)
import qualified Data.Text as T

instToText :: Inst -> Text
instToText i = case i of
  Nop       -> "nop"
  Push      -> "push"
  Drop      -> "drop"
  Icmp      -> "icmp"
  Jmp_if    -> "jmp_if"
  Jeq       -> "jeq"
  Jle       -> "jle"
  Jmp       -> "jmp"
  Call      -> "call"
  Ret       -> "ret"
  Imul      -> "imul"
  Idiv      -> "idiv"
  Iadd      -> "iadd"
  Isub      -> "isub"
  Decl      -> "decl"
  Read      -> "read"
  Read_a64  -> "read_a64"
  Write     -> "write"
  Write_a64 -> "write_a64"

allocArrayToText :: ArrayType -> Text
allocArrayToText at = case at of
  A8  -> "alloc_a8"
  A16 -> "alloc_a16"
  A32 -> "alloc_a32"
  A64 -> "alloc_a64"

initArrayToText :: ArrayType -> Text
initArrayToText at = case at of
  A8  -> "init_a8"
  A16 -> "init_a16"
  A32 -> "init_a32"
  A64 -> "init_a64"

allocV :: Text
allocV = "alloc_v"

initV :: Text
initV = "init_v"

toText :: Show a => a -> Text
toText = T.pack . show

programTextToText :: ProgramText -> Text
programTextToText ProgramText{..}
   = T.unlines
   $ ( "section bss"
     : map allocDirectiveToText programBssSeg)
  ++ ( "\nsection data"
     : map initDirectiveToText programDataSeg)
  ++ ( "\nsection text"
     : recTextSymbols 0 textSegSymbols programTextSeg)
  where
    recTextSymbols :: Word64 -> [(QLabel, Word64)] -> [Operator] -> [Text]

    recTextSymbols _ _ [] = []
    recTextSymbols _ [] ops
      = map (indent . operatorToText) ops
     ++ ["}"]

    recTextSymbols idx ((fname, 0):rest) (o:ops)
      = functionNameToText fname
      : "{"
      : indent (operatorToText o)
      : recTextSymbols (idx + 1) rest ops

    recTextSymbols idx same@((fname, addr):rest) (o:ops)
      | addr == idx = "}\n"
                    : functionNameToText fname
                    : "{"
                    : indent (operatorToText o)
                    : recTextSymbols (idx + 1) rest ops
      | otherwise   = indent (operatorToText o)
                    : recTextSymbols (idx + 1) same ops

    indent = ("  " <>)

functionNameToText :: QLabel -> Text
functionNameToText QLabel{..} = labelName

operatorToText :: Operator -> Text
operatorToText op = case op of
  Nullary i   -> instToText i
  Unary   i o -> instToText i <> " " <> operandToText o

operandToText :: Operand -> Text
operandToText o = case o of
  OperandNumber  w -> toText w
  OperandAddress a -> addressToText a

addressToText :: Address -> Text
addressToText a = case a of
  Symbolic q    -> qlabelToText q
  Numeric  s w  -> segTypeToText s <> ":" <> toText w

segTypeToText :: SegType -> Text
segTypeToText s = case s of
  Data  -> "d"
  Bss   -> "b"
  Text  -> "t"
  Local -> "l"

qlabelToText :: QLabel -> Text
qlabelToText QLabel{..}
   = T.intercalate "." labelQualifier
  <> segTypeToText labelSegType
  <> ":"
  <> labelName
  <> maybe
      ""
      ("." <>)
      labelRelative

allocDirectiveToText :: AllocDirective -> Text
allocDirectiveToText a = case a of
  AllocVar      -> allocV
  AllocArr at w -> allocArrayToText at
                <> " "
                <> toText w

initDirectiveToText  :: InitDirective -> Text
initDirectiveToText i = case i of
  InitVar   w  -> initV
               <> " "
               <> toText w
  InitArr t ws -> initArrayToText t
               <> " "
               <> (T.intercalate " " $ map toText ws)
