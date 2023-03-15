module ParserTypes where

import Data.Void
import Data.Text (Text)
import qualified Text.Megaparsec as P

type Stream = Text
type Parser = P.Parsec Void Stream
type ParserState = P.State Stream Void

