{-# LANGUAGE GADTs             #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Interpreter.Eval where 

import Prelude hiding (FilePath)


import           Data.Text
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Except

import           Interpreter.Type
import           Excel.Type 
import           Excel.Basics


-- TODO: Move most of these to thier own file

-- mapRight
passEither :: (a -> c) -> Either Text a -> Either Text c
passEither _ (Left e)  = Left e
passEither f (Right r) = Right (f r)

infixr 4 <?
(<?) :: (a -> c) -> Either Text a -> Either Text c
(<?) = passEither

apNewSheet :: [ArgType] -> Either Text NewExcelFileState
apNewSheet [TextType t] = Right (newSheet t)
apNewSheet []           = Left "No args"
apNewSheet _            = Left "Too many args"


createFuncMapping :: CreateFunc -> Either Text ToExec
createFuncMapping = 
  \case
    NewSheet x -> Direct <? apNewSheet x
    IDk        -> undefined

moveFuncMapping :: MoveFunc -> Either Text ToExec
moveFuncMapping = 
  \case 
    Up       -> undefined
    Down     -> undefined
    LLeft    -> undefined
    RRight   -> undefined
    ToCell x -> undefined


-- ENewExcelFileState => (ExcelFileState -> Either Text ExcelFileState)
-- Right (ENewExcelFileState) => Right (ExcelFileState -> Either Text ExcelFileState)
-- After fully applied this branch can be:
--    -> Right (Right x) 
--    -> Right (Left  e)
apSetActiveSheet :: [ArgType] -> Either Text ENewExcelFileState
apSetActiveSheet [TextType t] = Right (setActiveSheet t)
apSetActiveSheet []           = Left "No args"
apSetActiveSheet _            = Left "Too many args"


setFuncMapping :: SetFunc -> Either Text ToExec
setFuncMapping = 
  \case
    Constant      -> undefined
    ConstantRow   -> undefined
    ConstantCol   -> undefined
    ActiveSheet x -> Checked <? apSetActiveSheet x

insertFuncMapping :: InsertFunc -> Either Text ToExec
insertFuncMapping = undefined


newtype Eval a = Eval 
  { runEval :: StateT ExcelFileState (ExceptT Text IO) a 
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadState ExcelFileState
             , MonadError Text
             , MonadIO
             )


eval :: [Block t] -> Eval ()
eval (read:write:xs) =
  undefined

  where  
    -- TODO: Validate filepath
    evalReadFrom :: Block ('FileReadOp (Maybe Text)) -> Eval (Maybe Text)
    evalReadFrom (ReadFrom a) = case a of
                                  Nothing -> pure a
                                  -- validate here
                                  Just _  -> pure a
  
    -- TODO: Validate filepath
    evalWriteTo :: Block ('FileWriteOp Text) -> Eval Text
    evalWriteTo (WriteTo a) = maybe (throwError "File Write Error") pure a

    evalFunction :: Block ('Exec FuncType) -> Eval ToExec
    evalFunction (Function f) = 
      either throwError pure $ case f of
                                Create x -> createFuncMapping x 
                                Move x   -> moveFuncMapping x
                                Set x    -> setFuncMapping x
                                Insert x -> insertFuncMapping x

    evalDoBlock :: Block ('Section '[f]) -> Eval ExcelFileState
    evalDoBlock (DoBlock bs) = loop bs
      where loop (b':bs') = undefined
    
eval _ = undefined

    
tt :: Block ('Exec FuncType)
tt = Function (Insert (Value [TextType "2"]))

td :: Block ('Section '[Block ('Exec FuncType)])
td = DoBlock [tt]