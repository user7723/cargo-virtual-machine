module Assembler where

import Assembler.IR.Aliases
import Assembler.Translator

import qualified Data.ByteString as B
import Data.Set (Set)
import Control.Monad.Trans.Except

assemble :: ModuleName -> Set FilePath -> FilePath -> IO ()
assemble mn dirs out = do
  ep <- runExceptT $ readProgramText mn dirs
  case ep of
    Right p -> B.writeFile out $ translate p
    Left e  -> print e
