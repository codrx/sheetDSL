{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Interpreter.Eval where 

import Prelude hiding (FilePath)


import Data.Text (Text)
import Control.Monad.State
import Control.Monad.Reader

-- import Interpreter.Type
import Interpreter.Dispatch
import Excel.Type


-- this is disgustingly bad
-- need to redo syntax, parser, and design flow

-- type Stack = [DSLGrammar]

-- data Env = Env
--   { readF  :: Maybe (FileType, FilePath)
--   , writeF :: Maybe (FileType, FilePath)
--   }

-- data Interpreter a = Interpreter Env (StateT ExcelFileState IO a)

-- -- ReadFrom Identifier -> ReadFrom (File FileType FilePath)
-- unwrapFile :: Identifier -> (FileType, FilePath)
-- unwrapFile (File ft fp) = (ft, fp)
-- -- will redo later
-- unwrapFile _ = undefined

-- eval :: DSLGrammar -> Either ExcelFileState Env
-- eval (ReadFrom f) = Right (Env (Just (unwrapFile f)) Nothing)
-- eval (WriteTo f) = Right (Env Nothing (Just (unwrapFile f)))
-- eval (Function n a) = undefined
-- eval (WithinBlock r b) = undefined
-- eval (DoBlock b) = undefined


-- interpreter :: Stack -> Interpreter ()
-- interpreter = undefined




