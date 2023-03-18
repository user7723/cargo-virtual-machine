module Disassembler where

import Assembler.IR.Text

import Data.Serialize
import qualified Data.Text.IO as T
import qualified Data.ByteString as B

disassemble :: FilePath -> IO ()
disassemble fp = do
  esource <- decode <$> B.readFile fp
  case esource of
    Right ptext -> T.putStr $ programTextToText ptext
    Left str    -> putStrLn str
