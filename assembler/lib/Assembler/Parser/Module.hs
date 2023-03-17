module Assembler.Parser.Module where

import Assembler.IR.Aliases
import Assembler.IR.Module
import Assembler.IR.QLabel

import Assembler.Parser.Aliases
import Assembler.Parser.Lexer
import Assembler.Parser.ProgramGraph
import Assembler.Parser.QLabel

import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as P

import Data.Set (Set)
import qualified Data.Set as S

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
