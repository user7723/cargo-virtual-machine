module Assembler.Parser.Operator where

import Assembler.IR.FunctionDef
import Assembler.IR.InstructionSet
import Assembler.IR.Operator
import Assembler.IR.QLabel
import Assembler.IR.Text

import Assembler.Parser.Aliases
import Assembler.Parser.Lexer
import Assembler.Parser.Literals
import Assembler.Parser.QLabel

import Data.Maybe
import Control.Applicative (asum)
import qualified Data.Map as M

import Text.Megaparsec ((<?>), (<|>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P


parseInst :: FunctionDef -> Parser (Operator, FunctionDef)
parseInst fd = asum ((P.try . parseOperator fd) <$> [minBound .. maxBound])

parseOperator :: FunctionDef -> Inst -> Parser (Operator, FunctionDef)
parseOperator fd i = case i of
  Nop       -> nullary fd i
  Push      -> parsePush fd i
  Drop      -> nullary fd i
  Icmp      -> nullary fd i

  Jmp_if    -> unary   fd i controlFlowOperand
  Jeq       -> unary   fd i controlFlowOperand
  Jle       -> unary   fd i controlFlowOperand
  Jmp       -> unary   fd i controlFlowOperand

  Call      -> unary   fd i callOperand

  Ret       -> nullary fd i
  Imul      -> nullary fd i
  Idiv      -> nullary fd i
  Iadd      -> nullary fd i
  Isub      -> nullary fd i
  Decl      -> parseLocalDeclaration fd i
  Read      -> nullary fd i
  Read_a64  -> nullary fd i
  Write     -> nullary fd i
  Write_a64 -> nullary fd i

parseLocalDeclaration
  :: FunctionDef
  -> Inst
  -> Parser (Operator, FunctionDef)
parseLocalDeclaration fd i = do
  inst <- (i <$ keyword (instToText i))
  var  <- lexeme ident
  let idx = fromIntegral $ M.size (functionScope fd)
      fd' = insertLocal var idx fd
  pure (Nullary inst, fd')

nullary :: FunctionDef -> Inst -> Parser (Operator, FunctionDef)
nullary fd i
   =  ((, fd) . Nullary)
  <$> (i <$ keyword (instToText i))

unary
  :: FunctionDef
  -> Inst
  -> (FunctionDef -> Parser (Operand, FunctionDef))
  -> Parser (Operator, FunctionDef)
unary fd inst p
   =  (\i (o,s) -> (Unary i o, s))
  <$> (inst <$ keyword (instToText inst))
  <*> p fd

parsePush :: FunctionDef -> Inst -> Parser (Operator, FunctionDef)
parsePush fd i = do
  (op, fd') <- pushOperand fd
  pure (Unary i op, fd')

pushOperand :: FunctionDef -> Parser (Operand, FunctionDef)
pushOperand fd
    =  (,fd) <$> parseOperandNumber
   <|> parseOperandAddressReadable fd

parseOperandNumber :: Parser Operand
parseOperandNumber
   =  OperandNumber
  <$> (integralLiteral <|> charInitializer)

parseOperandAddressReadable
  :: FunctionDef
  -> Parser (Operand, FunctionDef)
parseOperandAddressReadable fd
   =  (P.try $ parseLocalVar fd)
  <|> (P.try $ (,fd) <$> parseNumericAddress Readable)
  <|> parseSymbolicAddress fd Readable

parseOperandAddressExecutable
  :: FunctionDef
  -> Parser (Operand, FunctionDef)
parseOperandAddressExecutable fd = parseSymbolicAddress fd Executable

parseLocalVar :: FunctionDef -> Parser (Operand, FunctionDef)
parseLocalVar fd = do
  l  <- parseSegType LocalScope <* P.char ':'
  o  <- P.getOffset
  mi <-  (findLocalIndex  fd <$> lexeme ident)
     <|> (validLocalIndex fd <$> integralLiteral)
  case mi of
    Just idx -> pure (OperandAddress $ Numeric l idx, fd)
    Nothing  -> do
      P.setOffset o
      fail "local variable used before declaration"

parseSymbolicAddress
  :: FunctionDef
  -> Access
  -> Parser (Operand, FunctionDef)
parseSymbolicAddress fd a = do
  mq <- P.optional $ parseQualifier <* P.char '.'
  s  <- parseSegType a
  _  <- P.char ':'
  i  <- lexeme ident
  ql <- pure $ QLabel (fromMaybe mn mq) s i Nothing
  pure (addr ql, insertDep ql fd)
  where
    mn = labelQualifier $ functionName fd
    addr = OperandAddress . Symbolic

parseNumericAddress :: Access -> Parser Operand
parseNumericAddress a = do
  s <- parseSegType a <|> parseSegTypeFromNumber a
  _ <- P.char ':'
  i <- integralLiteral
  pure $ addr s i
  where
    addr x y = OperandAddress $ Numeric x y

parseSegTypeFromNumber :: Access -> Parser SegType
parseSegTypeFromNumber a = do
  i <- integralLiteral <?> "<section index>"
  case numberToSegType a i of
    Just s  -> pure s
    Nothing -> fail
             $ "not an index of existing "
            ++ show a
            ++ " section: "
            ++ show i

parseSegType :: Access -> Parser SegType
parseSegType a =
  case a of
    LocalScope -> P.label "<local segment mnemonics>"
                $ Local <$ P.char 'l'
    Readable   ->  P.label "<readable segment mnemonics>"
                $  (Data  <$ P.char 'd')
               <|> (Bss   <$ P.char 'b')
    Executable -> P.label "<executable segment mnemonic>"
                $ Text <$ P.char 't'

callOperand :: FunctionDef -> Parser (Operand, FunctionDef)
callOperand fd = parseOperandAddressExecutable fd

controlFlowOperand :: FunctionDef -> Parser (Operand, FunctionDef)
controlFlowOperand fd = do
  l <- lexeme ident <?> "<local label>"
  pure (OperandAddress $ Symbolic $ fn { labelRelative = Just l } , fd)
  where
    fn = functionName fd
