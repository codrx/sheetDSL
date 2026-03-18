
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Interpreter.Type
    ( Identifier(..)
    , FilePath
    , FileType(..)
    , Argument
    , Range(..)
    , VarValueTy(..)
    , DSLGrammar(..)
    )
    where

import Prelude hiding (FilePath)
import Data.Text (Text)

type FilePath = Text

data FileType = 
    Excel 
  | CSV 
  deriving Show

data Identifier = 
    Variable Text 
  | FuncName Text
  | File FileType FilePath 
  deriving Show

data VarValueTy = 
    StringTy Text
  | IntegerTy Int
  | FractionalTy Double
  | BooleanTy Bool
  deriving Show

type Argument = Maybe [(Identifier, VarValueTy)]

data Range = 
    Row Text
  | Column Text
  | Selection Range Range
  deriving Show

data DSLGrammar = 
    ReadFrom Identifier
  | WriteTo Identifier
  | Function Identifier Argument
  | WithinBlock Range (Maybe [DSLGrammar])
  | DoBlock (Maybe [DSLGrammar])
  deriving Show


-- potential change in language structure to simplify Excel instructions
-- data BaseOp f =  
--     Set f
--   | Get f
--   | Move f
--   | Apply f
--   | Add f
--   | Minus f
--   | Replace f
--   | Insert f
--   | Delete f
--   | Fill f

