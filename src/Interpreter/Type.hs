
-- remove later
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE KindSignatures           #-}

module Interpreter.Type where


import           Prelude hiding (FilePath)

import           Data.Kind
import           Data.Text
import           Data.Proxy


data FileType =
    Excel
  | CSV


data VarValueType = 
    StringType Text
  | IntegerType Int
  | FractionalType Double
  | BooleanType Bool
  deriving Show


data FuncType f =  
    Move f
  | Set f
  -- | Get f
  -- | Apply f
  -- | Add f
  -- | Minus f
  -- | Replace f
  | Insert f
  -- | Delete f
  -- | Fill f
  deriving Show

-- example -> Function (Move Insert [Text "v"])

class ExeclF a
instance ExeclF (FuncType a)

class BlockF a
instance BlockF (FuncType a)

-- data Range = 
--     Row Text
--   | Column Text
--   | Selection Range Range
--   deriving Show

data Tags a =
    FileOp a
  | Exec a
  -- | WWithinBlock Range [FuncType a]
  | Section [a]
  deriving Show

-- why did i do it like this? beats me...
data Block :: Tags Type -> Type where
  ReadFrom    :: Text -> Block ('FileOp Text)
  WriteTo     :: Text -> Block ('FileOp Text)
  Function    :: ExeclF f => Proxy f -> Block ('Exec f)
  DoBlock     :: BlockF f => Proxy f -> Block ('Section t) -> Block ('Section (f ': t))
  -- WithinBlock :: Proxy f -> Block ('WWithinBlock r t) -> Block ('WWithinBlock r (f ': t))


instance Show (Block f) where
  show (ReadFrom t)  = "ReadFrom" ++ " " ++ show t
  show (WriteTo t)   = "WriteTo"  ++ " " ++ show t
  show (Function f)  = "Function" ++ " " ++ show f
  -- don't exactly know how to do DoBlock
  show (DoBlock _ b) = "Do" ++ " " ++ "[" ++ " " ++ show b  ++ " " ++ "]"
  -- show (WithinBlock f b) = undefined




