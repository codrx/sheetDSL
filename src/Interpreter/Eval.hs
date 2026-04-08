{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Interpreter.Eval where 

import Prelude hiding (FilePath)

import Interpreter.Type

import Data.Text (Text)
import Control.Monad.State
import Control.Monad.Reader

import Excel.Type


type Stack = [DSLGrammar]

data Env = Env
  { readF  :: Maybe (FileType, FilePath)
  , writeF :: (FileType, FilePath)
  }

data InterpreterState a = StateT ExcelFileState (IO a)


eval :: Stack -> InterpreterState ()
eval = undefined

-- (row, index) args will be fed using selection range
evalWithin = undefined




