{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}


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


{-
    Do I need to create NFData instance for this
-}
data ExcelFileState = ExcelFileState
  { _activeSheet :: Maybe (SheetName, Worksheet) 
  , _cellPos     :: CellCoord
  -- , _clipboard   :: Maybe ClipBoard
  , _xlsx        :: Xlsx
  } deriving (Eq, Show, Generic)

makeLenses ''ExcelFileState

instance Default ExcelFileState where
  def = ExcelFileState Nothing (RowAbs 1, ColumnAbs 1) def 


type NewExcelFileState = ExcelFileState -> ExcelFileState
type ENewExcelFileState = ExcelFileState -> Either Text ExcelFileState


data ToExec where
  Direct  :: (ExcelFileState -> ExcelFileState) -> ToExec 
  Checked :: (ExcelFileState -> Either Text ExcelFileState) -> ToExec 


execF :: ExcelFileState -> ToExec -> Either Text ExcelFileState
execF x (Direct f)  = pure (f x)
execF x (Checked f) = f x


-- class ChainableF f where
--   -- (-.>) :: MonadError e m => (f -> m ExcelFileState) -> (f -> m ExcelFileState) -> f -> m ExcelFileState 
--   (-.>) :: f -> f -> f -> ExcelFileState 
--   infixl 1 -.>


-- instance ChainableF (ToExec f) where 
--   (-.>) g f i  = undefined

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
