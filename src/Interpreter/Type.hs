
-- remove later
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE OverloadedStrings        #-}

module Interpreter.Type where


import           Prelude hiding (FilePath)

import           Data.Kind
import           Data.Text


-- TODO: use ?? i didnt't finish the sentence it seems like
data FileType =
    Excel
  | CSV


-- TODO: try out type families
data ArgType = 
    TextType Text
  | IntType Int
  | DoubleType Double
  | BooleanType Bool
  -- | ShortDateType Text
  -- | LongDateType Text
  -- | PercentageType Text
  -- | CurrencyType Text
  deriving Show

type Args = [ArgType]

-- function constraints
class CreateF f
class MovementF f
class SetF f
class InsertF f 

-- constrain 
data FuncType where
  Create  :: CreateFunc -> FuncType
  Move    :: MoveFunc   -> FuncType
  Set     :: SetFunc    -> FuncType
  Insert  :: InsertFunc -> FuncType
  -- Get     :: f -> FuncType f
  -- Select  :: f -> FuncType f
  -- Apply   :: f -> FuncType f
  -- Add     :: f -> FuncType f
  -- Minus   :: f -> FuncType f
  -- Replace :: f -> FuncType f
  -- Delete  :: f -> FuncType f
  -- Fill    :: f -> FuncType f

instance Show FuncType where
  show (Create f) = "Create" ++ " " ++ show f
  show (Move f)   = "Move"   ++ " " ++ show f
  show (Set f)    = "Set"    ++ " " ++ show f
  show (Insert f) = "Insert" ++ " " ++ show f

class ExcelF f
instance ExcelF FuncType

class BlockF f
instance BlockF FuncType


data CreateFunc = 
    NewSheet Args
  | IDk
  deriving Show
  
instance CreateF CreateFunc

data MoveFunc = 
    Up
  | Down
  | LLeft
  | RRight
  | ToCell Args
  deriving Show

instance MovementF MoveFunc

data SetFunc = 
    Constant
  | ConstantRow
  | ConstantCol
  | ActiveSheet Args
  deriving Show

instance SetF SetFunc


data InsertFunc =
    Value Args
  | Formula Args
  deriving Show

instance InsertF InsertFunc

-- TODO!
-- data Range = 
--     Row Text
--   | Column Text
--   | Selection Range Range
--   deriving Show


data Tag a =
    FileReadOp a
  | FileWriteOp a
  | Exec a
  | Section [a]
  -- | CondSection Range [a]
  deriving Show


data Block :: Tag Type -> Type where
  -- BlockNil    :: Block (Section '[])
  -- BlockCons   :: Block s -> Block ('Section x) -> Block ('Section (s ': x))
  ReadFrom    :: Maybe Text -> Block ('FileReadOp (Maybe Text))
  WriteTo     :: Maybe Text -> Block ('FileWriteOp (Maybe Text))
  --evalFunction won't work if its Exec (FuncType f)
  Function    :: (Show f, ExcelF f) => f -> Block ('Exec f)
  -- i'll constrain it tighter later
  DoBlock     :: (Show f) => [f] -> Block ('Section '[f])
  -- WithinBlock :: (Show b, ExcelF f) => Range -> Block ('Exec f) -> Block ('CondSection r '[b])

instance Show (Block f) where
  show (ReadFrom t) = "ReadFrom" ++ " " ++ show t
  show (WriteTo t)  = "WriteTo"  ++ " " ++ show t
  show (Function f) = "Function" ++ " " ++ show f
  show (DoBlock b)  = "Do" ++ " " ++ show b
