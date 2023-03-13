module Error where

import Module (ModuleName)
import Lexer (Stream)

import qualified Text.Megaparsec.Error as P
import Data.Void
import Data.Set (Set)


-- import Control.Exception.Base
import Control.Exception

type ParseErrorBundle = P.ParseErrorBundle Stream Void

data Error
  = ModuleWasNotFound ModuleName (Set FilePath)
  | ParseError ParseErrorBundle
  | IOError IOException
  deriving Show
