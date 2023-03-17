module Assembler.Error where

import Assembler.IR.Aliases
import Assembler.IR.QLabel

import Assembler.Parser.Aliases

import qualified Text.Megaparsec.Error as P
import Data.Void
import Data.Set (Set)

import Control.Exception

type ParseErrorBundle = P.ParseErrorBundle Stream Void

data Error
  = ModuleWasNotFound ModuleName (Set FilePath)
  | NoEntryPointWasGiven ModuleName FilePath
  | UndefinedReference QLabel
  | ParseError ParseErrorBundle
  | IOError IOException
  deriving Show
