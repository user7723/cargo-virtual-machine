module Assembler.Parser.StaticMemory where

import Assembler.IR.Aliases
import Assembler.IR.ProgramGraph
import Assembler.IR.StaticMemory
import Assembler.IR.QLabel
import Assembler.IR.Text

import Assembler.Parser.Aliases
import Assembler.Parser.Lexer
import Assembler.Parser.Literals

import Control.Applicative (asum)
import Data.Word

import qualified Data.Set as S
import qualified Data.Map as M


import Text.Megaparsec ((<?>), (<|>))
import qualified Text.Megaparsec as P

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
  _    <- keyword $ "alloc_a" <> showTextLower ty
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
  _  <- keyword $ "init_a" <> showTextLower ty
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
