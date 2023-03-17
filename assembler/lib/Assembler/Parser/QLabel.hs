module Assembler.Parser.QLabel where

import Assembler.IR.Aliases
import Assembler.IR.QLabel

import Assembler.Parser.Aliases
import Assembler.Parser.Lexer

import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

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
