{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}


module Excel.Type where

import           Data.Default
import           Data.Text (Text)

import           Control.Lens
import           GHC.Generics


import           Codec.Xlsx.Types



type SheetName = Text
type Row = Text
type Column = Text

-- newtype ClipBoard = ClipBoard { getStored :: forall a. a -> a } 

data ExcelFileState = ExcelFileState
  { _currentSheet :: (SheetName, Worksheet)
  , _cellPos      :: (Row, Column) 
  -- , _clipboard    :: Maybe ClipBoard
  , _xlsx         :: Xlsx
  } deriving (Eq, Show, Generic)

makeLenses ''ExcelFileState

type NewExcelFileState  = ExcelFileState -> ExcelFileState
type MNewExcelFileState = ExcelFileState -> Maybe ExcelFileState