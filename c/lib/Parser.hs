{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Parser where

import Text.Show.Pretty (pPrint) -- dbg

import Module

import Lexer
import Literals

import Data.Word
import Data.Void

import Control.Applicative (asum)
import Text.Megaparsec ((<?>), (<|>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

import qualified Data.Text as T
import qualified Data.Text.IO as T

parseQualifiedName :: Parser QualifiedName
parseQualifiedName = lexeme $ f <$> aux id ident (P.char '.')
  where
    f (q,n) = QualifiedName { qNameSpace = q [] , qName = n }
    aux acc p s = do
      x <- p
      ms <- P.optional s
      case ms of
        Nothing -> return (acc, x)
        Just _  -> aux (acc . (x :)) p s

parseName :: Parser Stream
parseName = lexeme (T.intercalate "." <$> P.sepBy1 ident (P.char '.'))

parseModuleName :: Parser Name
parseModuleName = keyword "module" *> (parseName <?> "<module name>")

parseModuleMain :: Parser (Maybe Name)
parseModuleMain = P.optional $ keyword "enter" *> parseName

parseExports :: Parser [ExportEntry]
parseExports = keyword "export" *> parens (commaSep parseExportEntry)

parseExportEntry :: Parser ExportEntry
parseExportEntry = ExportEntry <$> pname <*> parseParamsBlock
  where
    pname = lexeme (parseName <?> "<export entry name>")

parseParamsBlock :: Parser [Name]
parseParamsBlock
   = lexeme
   (  unit
  <|> single
  <|> parens (commaSep parseName))
  where
    unit = pure <$> symbol "()"
    single = pure <$> parseName

parseImportEntry :: Parser ImportEntry
parseImportEntry
   =  ImportEntry
  <$> (keyword "import" *> (parseName <?> "<module name>"))
  <*> P.optional (keyword "as" *> parseName)
  <*> parens (commaSep parseName)

parseImports :: Parser [ImportEntry]
parseImports = P.many parseImportEntry

parseHeader :: Parser Header
parseHeader
   =  Header
  <$> parseModuleName
  <*> parseModuleMain
  <*> parseExports
  <*> parseImports

parseBody :: Parser Body
parseBody
   =  Body
  <$> parseSectionBss
  <*> parseSectionData
  <*> parseSectionText

parseSectionBss :: Parser [AllocDirective]
parseSectionBss
  =  keyword "section"
  *> keyword "bss"
  *> P.many parseAllocDirective

parseAllocDirective :: Parser AllocDirective
parseAllocDirective = parseAllocVar <|> parseAllocArr

parseAllocVar :: Parser AllocDirective
parseAllocVar = do
  _   <- keyword "alloc_v"
  var <- parseName <?> "<variable>"
  return $ AllocVar var

parseAllocArr :: Parser AllocDirective
parseAllocArr = asum $ parseAllocArrOfType <$> [minBound .. maxBound]

parseAllocArrOfType :: ArrayType -> Parser AllocDirective
parseAllocArrOfType ty = do
  _    <- keyword $ "alloc_a" <> txtLower ty
  arrn <- parseName <?> "<arr name>"
  size <- integralLiteral <?> "<allocation size>"
  return $ AllocArr ty arrn size

parseSectionData :: Parser [InitDirective]
parseSectionData
  =  keyword "section"
  *> keyword "data"
  *> P.many parseInitDirective


parseInitDirective :: Parser InitDirective
parseInitDirective = parseInitVar <|> (asum $ parseInitArr <$> [minBound .. maxBound])

parseInitVar :: Parser InitDirective
parseInitVar = do
  _    <- keyword "init_v"
  name <- parseName <?> "<variable>"
  val  <- integralLiteral <|> charInitializer
  return $ InitVar name val


parseInitArr :: ArrayType -> Parser InitDirective
parseInitArr ty = do
  _  <- keyword $ "init_a" <> txtLower ty
  v  <- parseName <?> "<variable>"
  is <- stringArrayInitializer <|> integerArrayInitializer
  mapM_ validate is
  pure $ InitArr ty v is
  where
    validate i
      | i <= elemCapacity ty = pure ()
      | otherwise = fail
          (   show i
          ++ " cannot be used to initialize array with element size of "
          ++ show (elemCapacity ty :: Word64)
          )

parseSectionText :: Parser [FunctionDef]
parseSectionText
   =  keyword "section" *> keyword "text" *> P.some parseFunctionDef

parseFunctionDef :: Parser FunctionDef
parseFunctionDef
   =  FunctionDef
  <$> parseFunctionSignature
  <*> parseFunctionBody

parseFunctionSignature :: Parser FunctionSignature
parseFunctionSignature = do
  n  <- parseName <?> "<function name>"
  ps <- parseParamsBlock
  return $ FunctionSignature n ps

parseFunctionBody :: Parser FunctionBody
parseFunctionBody =
  P.between
    (symbol "{")
    (symbol "}")
    (P.many parseLabeledInst)

parseLabeledInst :: Parser LabeledOperator
parseLabeledInst
   =  LabeledOperator
  <$> labels
  <*> (asum (parseOperator <$> [minBound .. maxBound]))
  where
    labels = P.many label
    label  = P.label ".<label>" $ lexeme (P.between (P.char '.') (P.char ':') parseName)

parseOperator :: Inst -> Parser Operator
parseOperator i = case i of
  Nop        -> Nullary <$> inst i
  Ipush      -> Unary   <$> pure i <*> intOperand
  Icmp       -> Nullary <$> inst i
  Jmp_if     -> Unary   <$> inst i <*> nameOperand
  Jmp        -> Unary   <$> inst i <*> nameOperand
  Jeq        -> Unary   <$> inst i <*> nameOperand
  Jle        -> Unary   <$> inst i <*> nameOperand
  Call       -> Binary  <$> inst i <*> intOperand <*> qualified
  Ret        -> Nullary <$> inst i
  Imul       -> Nullary <$> inst i
  Idiv       -> Nullary <$> inst i
  Iadd       -> Nullary <$> inst i
  Isub       -> Nullary <$> inst i
  Idecl      -> Unary   <$> inst i <*> nameOperand
  Ibind      -> Unary   <$> inst i <*> nameOperand
  Iread      -> Unary   <$> inst i <*> nameOperand
  Iload      -> Unary   <$> inst i <*> qualified
  Iload_a64  -> Unary   <$> inst i <*> qualified
  Istore     -> Unary   <$> inst i <*> qualified
  Istore_a64 -> Unary   <$> inst i <*> qualified
  where
    intOperand  = OperandNumber <$> integralLiteral
    nameOperand = OperandName <$> parseName
    qualified   = OperandQualified <$> parseQualifiedName
    inst x = x <$ keyword (instToText x)

parseModule :: Parser Module
parseModule = Module <$> parseHeader <*> parseBody

parseModuleFromFile
  :: FilePath
  -> IO (Either (P.ParseErrorBundle Stream Void) Module)
parseModuleFromFile fp = do
  c <- T.readFile fp
  return $ P.parse parseModule fp c

parseModuleTest :: FilePath -> IO ()
parseModuleTest fp = do
  input <- T.readFile fp
  case P.parse parseModule fp input of
    Left e -> putStr (P.errorBundlePretty e)
    Right x -> pPrint x
