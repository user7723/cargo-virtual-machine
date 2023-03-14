{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Parser where

import Text.Show.Pretty (pPrint) -- dbg

import Module
import Lexer
import Literals
import Error
import ParserTypes

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


parseTest :: Show a => FilePath -> Parser a -> IO ()
parseTest fp p = do
  input <- T.readFile fp
  case P.parse p "" input of
    Left e -> putStr (P.errorBundlePretty e)
    Right x -> pPrint x


parseFromFile :: FilePath -> Parser a -> ExceptT Error IO a
parseFromFile fp p = do
  inp <- liftIO $ T.readFile fp
  case P.parse p fp inp of
    Left e -> throwE $ ParseError e
    Right r -> return r

parseMainFromFile :: FilePath -> ExceptT Error IO (Maybe QLabel)
parseMainFromFile fp = parseFromFile fp parseMain'
  where
    parseMain' = parseModuleName >>= parseModuleMain

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

parseExcept :: Monad m => Parser a -> FilePath -> Stream -> ExceptT Error m a
parseExcept p f input = except $
  case P.parse p f input of
    Left e  -> Left $ ParseError e
    Right r -> Right r

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

parseSegTypeQualifier :: Parser SegType
parseSegTypeQualifier
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
  secQ    <- P.try $ parseSegTypeQualifier <* P.char ':'
  lbl     <- ident
  pure $ QLabel modName secQ lbl Nothing

parseModule :: Parser Module
parseModule = do
  _     <- space
  mname <- parseModuleName
  entry <- parseModuleMain mname
  _     <- parseImports
  progr <- parseProgram mname
  pure $ Module
    { moduleName = mname
    , entryPoint = entry
    , programGraph = progr
    -- dependencies = deps
    }

parseProgram :: ModuleName -> Parser ProgramGraph
parseProgram mn = do
  bss <- parseSegTypeBss mn
  dat <- parseSegTypeData mn
  txt <- parseSegTypeText mn
  pure $ bss <> dat <> txt

parseModuleName :: Parser ModuleName
parseModuleName
  =  lexeme
  $  keyword "module"
  *> (parseQualifier <?> "<module name>")

parseModuleMain :: ModuleName -> Parser (Maybe QLabel)
parseModuleMain mn = lexeme $ do
  mmain <- P.optional (keyword "enter" *> ident)
  pure $ (\main -> QLabel mn Text main Nothing) <$> mmain

parseImportEntry :: Parser ModuleName
parseImportEntry
  =  lexeme
  $  keyword "import"
  *> parseQualifier

parseImports :: Parser (Set ModuleName)
parseImports = S.fromList <$> P.many parseImportEntry

parseSegTypeBss :: ModuleName -> Parser ProgramGraph
parseSegTypeBss m = do
  _   <- keyword "section" >> keyword "bss"
  qns <- P.many $ P.try $ parseNodeBss m
  pure $ M.fromList qns

-- data Node = Node Code (Set QLabel)
parseNodeBss :: ModuleName -> Parser (QLabel, Node)
parseNodeBss m = do
  i <- lexeme ident
  c <- BssCode <$> parseAllocDirective
  let ql = QLabel m Bss i Nothing
  pure (ql, Node c S.empty)

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

parseSegTypeData :: ModuleName -> Parser ProgramGraph
parseSegTypeData m = do
  _   <- keyword "section" >> keyword "data"
  qns <- P.many $ P.try $ parseNodeData m
  pure $ M.fromList qns

parseNodeData :: ModuleName -> Parser (QLabel, Node)
parseNodeData m = do
  i <- lexeme ident
  c <- DataCode <$> parseInitDirective
  let ql = QLabel m Data i Nothing
  pure (ql, Node c S.empty)

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

parseSegTypeText :: ModuleName -> Parser ProgramGraph
parseSegTypeText mn = do
  _   <- keyword "section" >> keyword "text"
  qns <- P.many $ parseNodeText mn
  pure $ M.fromList qns

parseNodeText :: ModuleName -> Parser (QLabel, Node)
parseNodeText mn = do
  fd <- parseFunctionDef mn
  pure (functionName fd, Node (TextCode fd) (functionDeps fd))

parseFunctionDef :: ModuleName -> Parser FunctionDef
parseFunctionDef mn = do
  fn <- lexeme ident <?> "<function name>"
  let ql = QLabel mn Text fn Nothing
      fd = emptyFunctionDef ql
  P.between
    (symbol "{")
    (symbol "}")
    (parseFunctionBody fd)

-- Attach parser state to each instruction being parsed
parseFunctionBody :: FunctionDef -> Parser FunctionDef
parseFunctionBody = aux 0
  where
    aux idx fd = do
      ls <- P.many parseLocalLabel
      let fd1 = foldr (\l m -> insertLabel l idx m) fd ls
      mia <- P.optional $ parseInst fd1
      case mia of
        Nothing        -> pure fd1
        Just (op, fd2) -> do
          let fd3 = insertUnresolved idx op $ insertInst idx op fd2
          aux (idx + 1) fd3

insertUnresolved :: Word64 -> Operator -> FunctionDef -> FunctionDef
insertUnresolved idx (Unary _ (OperandAddress (Symbolic _))) fd
  = let unres = S.insert idx $ functionUnresolved fd
    in fd { functionUnresolved = unres }
insertUnresolved _ _ fd = fd

parseLocalLabel :: Parser Label
parseLocalLabel = P.label ".<local label>:" $ lexeme $
  P.between (P.char '.') (P.char ':') ident

parseInst :: FunctionDef -> Parser (Operator, FunctionDef)
parseInst fd = asum ((P.try . parseOperator fd) <$> [minBound .. maxBound])

parseOperator :: FunctionDef -> Inst -> Parser (Operator, FunctionDef)
parseOperator fd i = case i of
  Nop       -> nullary fd i                     -- nullary : () -> ()
  Push      -> parsePush fd i                   -- nullary : ∀t.() -> (t)
  Drop      -> nullary fd i                     -- nullary : ∀t.(t) -> ()
  Icmp      -> nullary fd i                     -- nullary : (i64, i64) -> (i64)

  Jmp_if    -> unary   fd i controlFlowOperand  -- unary   : (i64) -> ()
  Jeq       -> unary   fd i controlFlowOperand  -- unary   : (i64) -> ()
  Jle       -> unary   fd i controlFlowOperand  -- unary   : (i64) -> ()
  Jmp       -> unary   fd i controlFlowOperand  -- unary   : () -> ()

  Call      -> unary   fd i callOperand  -- unary   : () -> ()

  Ret       -> nullary fd i                     -- nullary : () -> ()
  Imul      -> nullary fd i                     -- nullary : (i64, i64) -> (i64)
  Idiv      -> nullary fd i                     -- nullary : (i64, i64) -> (i64)
  Iadd      -> nullary fd i                     -- nullary : (i64, i64) -> (i64)
  Isub      -> nullary fd i                     -- nullary : (i64, i64) -> (i64)
  Decl      -> parseLocalDeclaration fd i       -- nullary
  Read      -> nullary fd i                     -- nullary : (addr:i64) -> (i64)
  Read_a64  -> nullary fd i                     -- nullary : (addr:i64, off:i64) -> (i64)
  Write     -> nullary fd i                     -- nullary : (addr:i64, val:i64) -> ()
  Write_a64 -> nullary fd i                     -- nullary : (addr:i64, off:i64, val:i64) -> ()

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
unary fd i p = (\i (o,s) -> (Unary i o, s)) <$> (i <$ keyword (instToText i)) <*> p fd

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
