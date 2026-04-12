{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}


module Excel.Type where

import           Data.Default
import           Data.Text (Text)

import           Control.Lens
import           GHC.Generics
import           GHC.Float (int2Double)


import           Codec.Xlsx.Types


type SheetName = Text
type Row = Text
type Column = Text

-- newtype ClipBoard = ClipBoard { getStored :: forall a. a -> a } 

data ExcelFileState = ExcelFileState
  { _activeSheet :: (SheetName, Worksheet) -- should i wrap in newtype?
  , _cellPos     :: CellCoord
  -- , _clipboard   :: Maybe ClipBoard
  , _xlsx        :: Xlsx
  } deriving (Eq, Show, Generic)

makeLenses ''ExcelFileState

instance Default ExcelFileState where
  def = undefined


type NewExcelFileState  = ExcelFileState -> ExcelFileState
type MNewExcelFileState = ExcelFileState -> Maybe ExcelFileState
type ENewExcelFileState = ExcelFileState -> Either () ExcelFileState


-- need to make most of these monads later

-- mimic Worksheet record from xlsx
-- could use GADTs
data WSContent = 
    WSColumn ([ColumnsProperties] -> [ColumnsProperties])
  | WSCells (CellMap -> CellMap)


-- mimic Cell record from xlsx
data CellContent = 
    CCStyle   Int
  | CCValue   CellValue
  | CCComment Comment
  | CCFormula CellFormula


class ValidCellValue a where
  toCellValue :: a -> CellContent

instance ValidCellValue Int where
  toCellValue = CCValue . review _CellDouble . int2Double

instance ValidCellValue Double where
  toCellValue = CCValue . review _CellDouble

instance ValidCellValue Bool where
  toCellValue = CCValue . review _CellBool

instance ValidCellValue Text where
  toCellValue = CCValue . review _CellText

