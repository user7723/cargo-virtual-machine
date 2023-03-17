module Assembler.IR.Aliases where

import Data.Text (Text)
import Data.Map (Map)

type Name       = Text
type Label      = Text
type ModuleName = [Text]
type (:->)      = Map
