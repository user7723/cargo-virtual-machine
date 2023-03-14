{-# LANGUAGE OverloadedStrings #-}

module Literals
  where

import Module
import Lexer
import ParserTypes

import Data.ReinterpretCast (wordToDouble)
import Data.Word

import Text.Megaparsec ((<?>), (<|>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

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

charToNum :: Num a => Char -> a
charToNum = fromIntegral . fromEnum

charInitializer :: Parser Word64
charInitializer = charToNum <$> charLiteral

stringArrayInitializer :: Parser [Word64]
stringArrayInitializer = (map charToNum) <$> stringLiteralNonEmpty

integerArrayInitializer :: Parser [Word64]
integerArrayInitializer = P.some (integralLiteral <|> charInitializer)
