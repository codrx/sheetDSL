
module Interpreter.Parser where

import           Data.Void (Void)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import           Text.Megaparsec 
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

