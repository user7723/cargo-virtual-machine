module Assembler.Parser.FunctionDef where

import Assembler.IR.Aliases
import Assembler.IR.FunctionDef
import Assembler.IR.ProgramGraph
import Assembler.IR.QLabel

import Assembler.Parser.Aliases
import Assembler.Parser.Lexer
import Assembler.Parser.Operator

import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

import qualified Data.Map as M

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

parseLocalLabel :: Parser Label
parseLocalLabel = P.label ".<local label>:" $ lexeme $
  P.between (P.char '.') (P.char ':') ident
