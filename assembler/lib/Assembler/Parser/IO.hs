module Assembler.Parser.IO where

import Text.Show.Pretty

import Assembler.IR.Aliases
import Assembler.IR.QLabel

import Assembler.Parser.Aliases
import Assembler.Parser.Lexer

import Assembler.Parser.Module
import Assembler.Parser.QLabel

import Assembler.Error

import Control.Monad.Trans.Except
import Control.Monad.IO.Class

import qualified Text.Megaparsec as P

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE

parseFromFile :: FilePath -> Parser a -> ExceptT Error IO a
parseFromFile fp p = do
  inp <- liftIO $ T.readFile fp
  case P.parse p fp inp of
    Left e -> throwE $ ParseError e
    Right r -> return r

parseTest :: Show a => FilePath -> Parser a -> IO ()
parseTest fp p = do
  input <- T.readFile fp
  case P.parse p "" input of
    Left e -> putStr (P.errorBundlePretty e)
    Right x -> pPrint x

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

    -- FIXME: absolutely horrible
    tokenModuleName
      = P.Tokens
      . NE.fromList
      . T.unpack
      . T.intercalate "."
