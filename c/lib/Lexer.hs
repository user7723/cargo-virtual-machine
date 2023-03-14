{-# LANGUAGE OverloadedStrings #-}

module Lexer where

import Module
import ParserTypes

import Data.Void
import Data.Proxy

import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Text (Text)

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

comma :: Parser Char
comma = lexeme $ P.char ','

commaSep :: Parser a -> Parser [a]
commaSep p = P.sepBy1 p comma

parens :: Parser a -> Parser a
parens = P.between (symbol "(") (symbol ")")

builtins :: [Name]
builtins = insts ++ tys
  where
    insts = map instToText [minBound .. maxBound]
    tys   = map instToText [minBound .. maxBound]

ident :: Parser Name
ident = do
  o <- P.getOffset
  t <- P.tokensToChunk (Proxy :: Proxy Stream)
    <$> ((:) <$> P.lowerChar <*> P.many P.alphaNumChar)
  if (not $ t `elem` builtins)
  then pure t
  else do
    P.setOffset o
    fail "<identifier>" -- FIXME: fail -> failure

capIdent :: Parser Name
capIdent = do
  o <- P.getOffset
  t <- P.tokensToChunk (Proxy :: Proxy Stream)
    <$> ((:) <$> P.upperChar <*> P.many P.letterChar)
  if (not $ t `elem` builtins)
  then pure t
  else do
    P.setOffset o
    fail "<identifier>" -- FIXME: fail -> failure
