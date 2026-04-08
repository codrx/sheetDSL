{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}


module Excel.Type where

import           Data.Default
import           Data.Text (Text)

import           Control.Lens
import           GHC.Generics


import           Codec.Xlsx.Types
import           Codec.Xlsx.Types.Common



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

type NewExcelFileState  = ExcelFileState -> ExcelFileState
type MNewExcelFileState = ExcelFileState -> Maybe ExcelFileState

class GoesInCell a where
  isCellRef     :: a -> Maybe CellRef
  isCellValue   :: a -> Maybe CellValue
  isCellFormula :: a -> Maybe CellFormula

instance GoesInCell Int where
  isCellValue a = undefined

