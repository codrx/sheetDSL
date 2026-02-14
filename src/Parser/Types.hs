{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}


{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Parser.Types 
  ( Parser
  , FArgs
  , Var
  , End(..)
  , Stmt(..)
  ) where

import Data.Void (Void)
import Data.Text (Text)
import Control.Monad.Reader (ReaderT)
import Control.Lens (makeLenses)

import Text.Megaparsec (Parsec)


type Parser = Parsec Void Text

type Var = Text
type FArgs = Maybe [(Var, Text)]

data End = End deriving Show

data Stmt a where
  Src         :: Text -> Stmt a
  Dest        :: Text -> Stmt a
  Do          :: [Stmt a] -> End -> Stmt a
  BuiltInFunc :: Text -> FArgs -> Stmt a
  EmptyScope  :: Stmt a


data DXLFilePath = 
  DxlFilePath
    { _fileToRead  :: Maybe Text
    , _fileToWrite :: Maybe Text
    }

-- temporary fields
data DXLState = 
  DXLState 
    { _excelStateRow    :: Int 
    , _excelStateCol    :: Int 
    , _dxlAST           :: [Stmt Text]
    }

makeLenses ''DXLState

type DXL = ReaderT DXLFilePath IO DXLState


cShowL :: Show a => [Stmt a] -> String
cShowL xs = "[ " ++ go xs ++ " ]"
  where
    go [] = []
    go [y] = show y
    go (y:ys) = show y ++ ", " ++ go ys

instance Show a => Show (Stmt a) where 
  show (Src t)       = "Source:" ++ " " ++ show t 
  show (Dest t)            = "Destination:" ++ " " ++ show t 
  show (Do xs e)     = "Do " ++ cShowL xs ++ " " ++
  show e show (BuiltInFunc t arg) = "BuiltinFunc " ++ show t ++ " " ++ show arg
  show EmptyScope    = "EmptyScope"