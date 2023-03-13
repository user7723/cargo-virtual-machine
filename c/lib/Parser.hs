{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Parser where

import Text.Show.Pretty (pPrint) -- dbg

import Module
import Lexer
import Literals
import Error

import Data.Word
import Data.Void
import Data.Char
import Data.Maybe

import Control.Applicative (asum)
import Text.Megaparsec ((<?>), (<|>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

import qualified Text.Megaparsec.Error as P

import qualified Data.List.NonEmpty as NE

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Set (Set)
import qualified Data.Set as S

import Data.Map (Map)
import qualified Data.Map as M

import Control.Monad.Trans.Except
import Control.Monad.IO.Class

parseFromFile :: FilePath -> Parser a -> ExceptT Error IO a
parseFromFile fp p = do
  inp <- liftIO $ T.readFile fp
  case P.parse p fp inp of
    Left e -> throwE $ ParseError e
    Right r -> return r

parseImportsFromFile :: ModuleName -> FilePath -> ExceptT Error IO (Set ModuleName)
parseImportsFromFile m fp = parseFromFile fp parseImports'
  where
    parseImports' = do
      _  <- keyword "module"
      o  <- P.getOffset
      m' <- lexeme $ parseQualifier
      if m /= m'
      then do
        P.setOffset o
        P.failure
          (Just $ tokenModuleName m')
          (S.singleton $ tokenModuleName m)
      else parseModuleMain m' >> parseImports

    tokenModuleName
      = P.Tokens
      . NE.fromList
      . T.unpack
      . T.intercalate "."

parseQualifier :: Parser ModuleName
parseQualifier = ($ []) <$> aux id
  where
    aux acc = do
      i <- capIdent
      let acc' = acc . (i:)
      undo <- P.getParserState
      m <- P.optional $ P.try (P.char '.' *> P.upperChar)
      P.setParserState undo
      case m of
        Just _  -> P.char '.' >> aux acc'
        Nothing -> pure acc'

parseSectionQualifier :: Parser Section
parseSectionQualifier
   =  (Data     <$ P.char 'd')
  <|> (Bss      <$ P.char 'b')
  <|> (Text     <$ P.char 't')
  <|> (Local    <$ P.char 'l')
-- it's only possible to acquire absolute address
-- by running some c-functions e.g. malloc etc.
--  <|> (Absolute <$ P.char 'a')

parseQLabel :: Parser QLabel
parseQLabel = lexeme $ do
  modName <- parseQualifier
  _       <- P.char '.'
  secQ    <- P.try $ parseSectionQualifier <* P.char ':'
  lbl     <- ident
  pure $ QLabel modName secQ lbl

parseModuleName :: Parser ModuleName
parseModuleName
  =  lexeme
  $  keyword "module"
  *> (parseQualifier <?> "<module name>")

parseModuleMain :: ModuleName -> Parser (Maybe QLabel)
parseModuleMain mn
  = lexeme
  $ P.optional
  $ QLabel mn Text <$> (keyword "enter" *> ident)

parseImportEntry :: Parser ModuleName
parseImportEntry
  =  lexeme
  $  keyword "import"
  *> parseQualifier

parseImports :: Parser (Set ModuleName)
parseImports = S.fromList <$> P.many parseImportEntry

parseSectionBss :: ModuleName -> Parser ProgramGraph
parseSectionBss m = do
  _   <- keyword "section" >> keyword "bss"
  qns <- P.many $ parseNodeBss m
  pure $ M.fromList qns

-- data Node = Node Code (Set QLabel)
parseNodeBss :: ModuleName -> Parser (QLabel, Node)
parseNodeBss m = do
  i <- lexeme ident
  c <- BssCode <$> parseAllocDirective
  let ql = QLabel m Bss i
  pure (ql, Node c $ S.empty)

parseAllocDirective :: Parser AllocDirective
parseAllocDirective = parseAllocVar <|> parseAllocArr

parseAllocVar :: Parser AllocDirective
parseAllocVar = AllocVar <$ keyword "alloc_v"

parseAllocArr :: Parser AllocDirective
parseAllocArr = asum $ parseAllocArrOfType <$> [minBound .. maxBound]

parseAllocArrOfType :: ArrayType -> Parser AllocDirective
parseAllocArrOfType ty = do
  _    <- keyword $ "alloc_a" <> txtLower ty
  size <- integralLiteral <?> "<allocation size>"
  return $ AllocArr ty size

parseSectionData :: ModuleName -> Parser ProgramGraph
parseSectionData m = do
  _   <- keyword "section" >> keyword "data"
  qns <- P.many $ parseNodeData m
  pure $ M.fromList qns

parseNodeData :: ModuleName -> Parser (QLabel, Node)
parseNodeData m = do
  i <- lexeme ident
  c <- DataCode <$> parseInitDirective
  let ql = QLabel m Data i
  pure (ql, Node c $ S.empty)

parseInitDirective :: Parser InitDirective
parseInitDirective
   =  parseInitVar
  <|> (asum $ parseInitArr <$> [minBound .. maxBound])

parseInitVar :: Parser InitDirective
parseInitVar = do
  _    <- keyword "init_v"
  val  <- integralLiteral <|> charInitializer
  return $ InitVar val

parseInitArr :: ArrayType -> Parser InitDirective
parseInitArr ty = do
  _  <- keyword $ "init_a" <> txtLower ty
  is <- stringArrayInitializer <|> integerArrayInitializer
  mapM_ validate is
  pure $ InitArr ty is
  where
    validate i
      | i <= elemCapacity ty = pure ()
      | otherwise = fail
          (   show i
          ++ " cannot be used to initialize array with element size of "
          ++ show (elemCapacity ty :: Word64)
          )

parseNodeText :: ModuleName -> Parser (QLabel, Node)
parseNodeText mn = do
  (fd, deps) <- parseFunctionDef mn
  pure (functionName fd, Node (TextCode fd) deps)

parseFunctionDef :: ModuleName -> Parser (FunctionDef, Set QLabel)
parseFunctionDef mn = do
  fn <- lexeme ident <?> "<function name>"
  let ql = QLabel mn Text fn
  (fb, acc') <- P.between
      (symbol "{")
      (symbol "}")
      (parseFunctionBody (S.empty) mn mempty)
  pure (FunctionDef ql fb, acc')

parseFunctionBody
  :: Set QLabel
  -> ModuleName
  -> FunctionBody
  -> Parser (FunctionBody, Set QLabel)
parseFunctionBody = aux 0
  where
    aux idx acc mn fb = do
      ls <- P.many parseLocalLabel
      let fb' = foldr (\l m -> insertLabel l idx m) fb ls
      mia <- P.optional $ parseInst acc mn
      case mia of
        Nothing           -> pure (fb', acc)
        Just (inst, acc') -> do
          let fb'' = insertInst idx inst fb'
          aux (idx + 1) acc' mn fb

parseLocalLabel :: Parser Label
parseLocalLabel = P.label ".<local label>:" $ lexeme $
  P.between (P.char '.') (P.char ':') ident

parseInst :: Set QLabel -> ModuleName -> Parser (Operator, Set QLabel)
parseInst acc mn = asum (parseOperator acc mn <$> [minBound .. maxBound])

parseOperator
 :: Set QLabel
 -> ModuleName
 -> Inst
 -> Parser (Operator, Set QLabel)
parseOperator acc mn i = case i of
  Nop       -> nullary i -- nullary : () -> ()
  Push      -> unary i (pushOperand acc mn) -- nullary : ∀t.() -> (t)
  Icmp      -> nullary i -- nullary : (i64, i64) -> (i64)

  Jmp_if    -> unary i (controlFlowOperand acc mn)  -- unary   : (i64) -> ()
  Jeq       -> unary i (controlFlowOperand acc mn)  -- unary   : (i64) -> ()
  Jle       -> unary i (controlFlowOperand acc mn)  -- unary   : (i64) -> ()
  Jmp       -> unary i (controlFlowOperand acc mn)  -- unary   : () -> ()
  Call      -> unary i (controlFlowOperand acc mn)  -- unary   : () -> ()

  Ret       -> nullary i -- nullary : () -> ()
  Imul      -> nullary i -- nullary : (i64, i64) -> (i64)
  Idiv      -> nullary i -- nullary : (i64, i64) -> (i64)
  Iadd      -> nullary i -- nullary : (i64, i64) -> (i64)
  Isub      -> nullary i -- nullary : (i64, i64) -> (i64)
  Decl      -> nullary i -- nullary
  Read      -> nullary i -- nullary : (addr:i64) -> (i64)
  Read_a64  -> nullary i -- nullary : (addr:i64, off:i64) -> (i64)
  Write     -> nullary i -- nullary : (addr:i64, val:i64) -> ()
  Write_a64 -> nullary i -- nullary : (addr:i64, off:i64, val:i64) -> ()

nullary :: Inst -> Parser (Operator, Set QLabel)
nullary i = ((, S.empty) . Nullary) <$> (i <$ keyword (instToText i))

unary
  :: Inst
  -> Parser (Operand, Set QLabel)
  -> Parser (Operator, Set QLabel)
unary i p = (\i (o,s) -> (Unary i o, s)) <$> (i <$ keyword (instToText i)) <*> p

pushOperand :: Set QLabel -> ModuleName -> Parser (Operand, Set QLabel)
pushOperand acc mn
    =  (,S.empty) <$> parseOperandNumber
   <|> parseOperandAddress acc Readable mn

parseOperandNumber :: Parser Operand
parseOperandNumber
   =  OperandNumber
  <$> (integralLiteral <|> charInitializer)

parseOperandAddress :: Set QLabel -> Access -> ModuleName -> Parser (Operand, Set QLabel)
parseOperandAddress acc a mn
   =  (P.try $ parseSymbolicAddress acc a mn)
  <|> (,acc) <$> parseNumericAddress a

parseSymbolicAddress
  :: Set QLabel
  -> Access
  -> ModuleName
  -> Parser (Operand, Set QLabel)
parseSymbolicAddress acc a mn = do
  mq <- P.optional $ parseQualifier <* P.char '.'
  s  <- parseSection a
  _  <- P.char ':'
  i  <- lexeme ident
  ql <- pure $ QLabel (fromMaybe mn mq) s i
  pure (addr ql, S.insert ql acc)
  where
    addr = OperandAddress . Symbolic

parseNumericAddress :: Access -> Parser Operand
parseNumericAddress a = do
  s  <- parseSection a <|> parseSectionFromNumber a
  _  <- P.char ':'
  i  <- integralLiteral
  pure $ addr s i
  where
    addr x y = OperandAddress $ Numeric x y

parseSectionFromNumber :: Access -> Parser Section
parseSectionFromNumber a = do
  i <- integralLiteral <?> "<section index>"
  case numberToSection a i of
    Just s  -> pure s
    Nothing -> fail
             $ "not an index of existing "
            ++ show a
            ++ " section: "
            ++ show i

parseSection :: Access -> Parser Section
parseSection a =
  case a of
    Readable ->  P.label "<readable segment mnemonics>"
              $  (Data  <$ P.char 'd')
             <|> (Bss   <$ P.char 'b')
             <|> (Local <$ P.char 'l')
    Executable -> P.label "<executable segment mnemonic>"
                $ Text <$ P.char 't'

controlFlowOperand :: Set QLabel -> ModuleName -> Parser (Operand, Set QLabel)
controlFlowOperand acc mn = parseOperandAddress acc Executable mn
