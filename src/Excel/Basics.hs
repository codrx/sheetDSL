{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Excel.Basics where


import Data.Text (Text)
import Data.Foldable (foldl')
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import qualified Data.ByteString.Lazy as BL
import Codec.Xlsx
import Control.Lens


readExcelFile :: String -> IO Xlsx
readExcelFile f = do
  bs <- BL.readFile f
  return $ toXlsx bs


writeExcelFile :: Xlsx -> FilePath -> IO ()
writeExcelFile xlsx f = do
  ct <- getPOSIXTime
  BL.writeFile f $ fromXlsx ct xlsx


data CellSelection = CellSelection
  { start :: Int
  , end   :: Int
  }

-- m [functions] selection


-- each function should return new Xlsx
sheetWriter :: Xlsx -> [Xlsx -> Xlsx] -> Xlsx
sheetWriter = foldl' (flip ($))

cellWriter :: Worksheet -> [Worksheet -> Worksheet] -> Worksheet
cellWriter = foldl' (flip ($))

{-# INLINE cellReader #-}
cellReader :: Traversable t => forall a. t f -> Worksheet -> a
cellReader = undefined

-- Creates and moves focus to new sheet
newSheet :: Text -> (Xlsx -> Xlsx)
newSheet sheetName = atSheet sheetName ?~ def

setCurrentSheet = undefined

copy = undefined

paste = undefined

rowApply = undefined

columnApply = undefined

getColumnValue = undefined

getRowValue = undefined

addColumnValue = undefined

addRowValue = undefined

replaceColumnValue = undefined

replaceRowValue = undefined

insertColumn = undefined

insertRow = undefined

deleteColumn = undefined

deleteRow = undefined

test = def & newSheet "A"
