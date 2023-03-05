{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Parser where

import Module
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
  -- , Constant(..)
  , FunctionBody
  , LabeledOperator(..)
  , StaticMemoryInst(..)
  , Inst(..)
  , Operator(..)
  , Operand(..)
  , typeToText
  , instToText
  )

import Data.ReinterpretCast (wordToDouble)

import Data.Word
import Data.Void
import Data.Proxy
import Data.Bifunctor (bimap)

import Control.Applicative (asum)
import Text.Megaparsec ((<?>), (<|>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

type Stream = Text
type Parser = P.Parsec Void Stream

lineComment :: Stream
lineComment = "//"

blockCommentStart :: Stream
blockCommentStart = "/*"

blockCommentEnd :: Stream
blockCommentEnd = "*/"

space :: Parser ()
space = L.space
  P.space1
  (L.skipLineComment lineComment)
  (L.skipBlockComment blockCommentStart blockCommentEnd)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

keyword :: Name -> Parser Name
keyword k = lexeme (P.string k <* P.notFollowedBy P.alphaNumChar)

symbol :: Stream -> Parser Stream
symbol = L.symbol space

proxy :: Proxy Stream
proxy = Proxy

builtins :: [Name]
builtins = insts ++ tys
  where
    insts = map instToText [minBound .. maxBound]
    tys   = map typeToText [minBound .. maxBound]

ident :: Parser Name
ident = do
  o <- P.getOffset
  t <- P.tokensToChunk proxy <$> P.some P.alphaNumChar
  if (not $ t `elem` builtins)
  then pure t
  else do
    P.setOffset o
    fail "<identifier>"

parseName :: Parser Stream
parseName = lexeme (T.intercalate "." <$> P.sepBy1 ident (P.char '.'))

parseModuleName :: Parser Name
parseModuleName = keyword "module" *> (parseName <?> "<module name>")

parseModuleMain :: Parser (Maybe Name)
parseModuleMain = P.optional $ keyword "enter" *> parseName

parseExports :: Parser [ExportEntry]
parseExports = keyword "export" *> parens (commaSep parseExportEntry)

parseExportEntry :: Parser ExportEntry
parseExportEntry = ExportEntry <$> pname <*> ptype
  where
    pname = lexeme (parseName <?> "<export entry name>")
    ptype = symbol ":" *> parseType

parseType :: Parser Type
parseType
   =  (FunctTy <$> parseFunctionalType)
  <|> (ConstTy <$> parseConstantType)

parseConstantType :: Parser ConstantType
parseConstantType
  =   (I64  <$ symbol "i64")
  <|> (F64  <$ symbol "f64")
  <|> (Unit <$ symbol "()")

comma :: Parser Char
comma = lexeme $ P.char ','

commaSep :: Parser a -> Parser [a]
commaSep p = P.sepBy1 p comma

parens :: Parser a -> Parser a
parens = P.between (symbol "(") (symbol ")")

parseTypes :: Parser [ConstantType]
parseTypes =  fmap pure parseConstantType
          <|> parens (commaSep parseConstantType)

parseFunctionalType :: Parser FunctionalType
parseFunctionalType
   =  FunctionalType
  <$> parseTypes
  <*> (symbol "->" *> parseTypes)

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
  = (  keyword "section"
    *> keyword "bss"
    *> P.many parseAllocDirective
    ) <|> pure []

parseAllocDirective :: Parser AllocDirective
parseAllocDirective
   =  (AllocDirectiveV <$> parseAllocVar)
  <|> (AllocDirectiveA <$> parseAllocArray)

parseAllocVar :: Parser AllocVar
parseAllocVar
   =  AllocVar
  <$> parseAllocVarType
  <*> (parseName <?> "<symbol>")

parseAllocVarType :: Parser ConstantType
parseAllocVarType
   =  (I64 <$ symbol "alloc_i64")
  <|> (F64 <$ symbol "alloc_f64")

signed :: Num a => Parser a -> Parser a
signed = L.signed P.space

decimal :: Parser Word64
decimal = lexeme (signed L.decimal) <?> "exepected: <integer>"

hexadecimal :: Parser Word64
hexadecimal = lexeme (pref *> L.hexadecimal) <?> "<hex value>"
  where pref = P.string "0x" <|> P.string "0X"

octal :: Parser Word64
octal = lexeme (pref *> L.octal) <?> "<octal value>"
  where pref = P.string "0o" <|> P.string "0O"

binary :: Parser Word64
binary = lexeme (pref *> L.binary) <?> "<binary value>"
  where pref = P.string "0b" <|> P.string "0B"

float :: Parser Double
float = lexeme (signed L.float) <?> "<floating point literal>"

integralLiteral :: Parser Word64
integralLiteral
   =  hexadecimal
  <|> octal
  <|> binary
  <|> decimal

floatingLiteral :: Parser Double
floatingLiteral
   =  (wordToDouble <$> integralLiteral)
  <|> float

parseAllocArray :: Parser AllocArray
parseAllocArray
   =  AllocArray
  <$> parseArrayType
  <*> (parseName <?> "<symbol>")
  <*> (integralLiteral <?> "<allocation size>")

charLiteral :: Parser Char
charLiteral
  = P.label "<char literal>"
  $ lexeme (P.between (P.char '\'') (P.char '\'') L.charLiteral)

stringLiteral :: Parser [Char]
stringLiteral = P.label "<string literal>"
  (P.char '\"' *> P.manyTill L.charLiteral (P.char '\"'))

stringLiteralNonEmpty :: Parser [Char]
stringLiteralNonEmpty = P.label "<string literal>"
  (P.char '\"' *> P.someTill L.charLiteral (P.char '\"'))

parseArrayType :: Parser ArrayType
parseArrayType
   =  (A8  <$ symbol "alloc_a8")
  <|> (A16 <$ symbol "alloc_a16")
  <|> (A32 <$ symbol "alloc_a32")
  <|> (A64 <$ symbol "alloc_a64")

parseSectionData :: Parser [InitDirective]
parseSectionData
  = (  keyword "section"
    *> keyword "data"
    *> P.many parseInitDirective
    ) <|> pure []

parseInitDirective :: Parser InitDirective
parseInitDirective
   =  (InitDirectiveV <$> parseInitDirectiveVar)
  <|> (InitDirectiveA <$> parseInitDirectiveArray)

parseInitDirectiveVar :: Parser InitDirectiveVar
parseInitDirectiveVar
   =  (InitI <$> parseInitIntegral)
  <|> (InitF <$> parseInitFloating)

parseInitIntegral :: Parser InitIntegral
parseInitIntegral
   =  InitIntegral
  <$> (keyword "init_i64" *> pure I64)
  <*> (parseName <?> "<variable>")
  <*> (integralLiteral <|> charInitializer)

parseInitFloating :: Parser InitFloating
parseInitFloating
   =  InitFloating
  <$> (keyword "init_f64" *> pure F64)
  <*> (parseName <?> "<variable>")
  <*> floatingLiteral

parseInitDirectiveArray :: Parser InitDirectiveArray
parseInitDirectiveArray
   =  parseArrayInit A8
  <|> parseArrayInit A16
  <|> parseArrayInit A32
  <|> parseArrayInit A64

charToNum :: Num a => Char -> a
charToNum = fromIntegral . fromEnum

charInitializer :: Parser Word64
charInitializer = charToNum <$> charLiteral

stringArrayInitializer :: Parser [Word64]
stringArrayInitializer = (map charToNum) <$> stringLiteralNonEmpty

integerArrayInitializer :: Parser [Word64]
integerArrayInitializer = P.some (integralLiteral <|> charInitializer)

elemCapacity :: Num a => ArrayType -> a
elemCapacity A8  = fromIntegral (maxBound :: Word8)
elemCapacity A16 = fromIntegral (maxBound :: Word16)
elemCapacity A32 = fromIntegral (maxBound :: Word32)
elemCapacity A64 = fromIntegral (maxBound :: Word64)

arrayTypeToText :: ArrayType -> Text
arrayTypeToText A8 = "a8"
arrayTypeToText A16 = "a16"
arrayTypeToText A32 = "a32"
arrayTypeToText A64 = "a64"

parseArrayInit :: ArrayType -> Parser InitDirectiveArray
parseArrayInit ty = do
  _  <- keyword ("init_" <> arrayTypeToText ty)
  v  <- parseName <?> "<variable>"
  is <- stringArrayInitializer <|> integerArrayInitializer
  mapM_ validate is
  pure $ InitDirectiveArray ty v is
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
  <|> pure []

parseFunctionDef :: Parser FunctionDef
parseFunctionDef
   =  FunctionDef
  <$> parseFunctionSignature
  <*> parseFunctionBody


parseFunctionSignature :: Parser FunctionSignature
parseFunctionSignature = do
  n       <- parseName <?> "<function name>"
  _       <- symbol ":"
  (ts,ps) <- parseFunctionDefTypeBlock
  return $ FunctionSignature n ts ps

parseFunctionDefTypeBlock :: Parser (FunctionalType, [Parameter])
parseFunctionDefTypeBlock
   =  (\(ts,ps) -> (,ps) . FunctionalType ts)
  <$> (unit <|> single <|> tuple )
  <*> (symbol "->" *> parseTypes)
  where
    unit = ([Unit],[]) <$ symbol "()"
    single :: Parser ([ConstantType], [Parameter])
    single = (bimap pure pure) <$> param

    tuple :: Parser ([ConstantType], [Parameter])
    tuple = unzip <$> parens (commaSep param)

    param :: Parser (ConstantType, Parameter)
    param =  (\pn ty -> (ty, Parameter pn ty))
         <$> (parseName <?> "<parameter>")
         <*> (symbol ":" *> parseConstantType)

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
  Nop            -> Nullary <$> inst i
  Push_i64       -> Unary   <$> pure i <*> (intOperand <* symbol ":" <* symbol "i64")
  Cmp_i64        -> Nullary <$> inst i
  Jmp_if         -> Unary   <$> inst i <*> nameOperand
  Jmp            -> Unary   <$> inst i <*> nameOperand
  Jeq            -> Unary   <$> inst i <*> nameOperand
  Jle            -> Unary   <$> inst i <*> nameOperand
  Call           -> Binary  <$> inst i <*> intOperand <*> nameOperand
  Ret            -> Nullary <$> inst i
  Mul_i64        -> Nullary <$> inst i
  Div_i64        -> Nullary <$> inst i
  Add_i64        -> Nullary <$> inst i
  Sub_i64        -> Nullary <$> inst i
  Local_decl_i64 -> Unary   <$> inst i <*> nameOperand
  Local_bind_i64 -> Unary   <$> inst i <*> nameOperand
  Local_read_i64 -> Unary   <$> inst i <*> nameOperand
  Load_i64       -> Unary   <$> inst i <*> nameOperand
  Load_a64_i64   -> Unary   <$> inst i <*> nameOperand
  Store_i64      -> Unary   <$> inst i <*> nameOperand
  Store_a64_i64  -> Unary   <$> inst i <*> nameOperand
  where
    intOperand  = OperandNumber <$> integralLiteral
    nameOperand = OperandName <$> parseName
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
  c <- T.readFile fp
  P.parseTest parseModule c
