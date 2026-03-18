{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}

module Excel.Basics
  ( copy
  , paste
  , newSheet
  , setActiveSheet
  , moveToCell
  , rowApply
  , columnApply
  ) where

import           Excel.Type

import           Data.Text (Text)
import           Data.Time.Clock.POSIX

import qualified Data.ByteString.Lazy as BL

import           Codec.Xlsx
import           Control.Lens


{-

  Majority of functions overwrite existing cell contents

  - may need to wrap accessors in newtype

-}



copy = undefined

_getCellData = undefined

paste = undefined

newSheet :: SheetName -> NewExcelFileState 
newSheet sheetName = xlsx . atSheet sheetName ?~ def

setActiveSheet :: SheetName -> MNewExcelFileState
setActiveSheet sheetName efs = do 
  newWS <- efs ^? xlsx . ixSheet sheetName
  let (sn, ws)   = efs ^. currentSheet
      saveCurrWS = xlsx . ixSheet sn .~ ws
      setNewWS   = currentSheet .~ (sheetName, newWS)
  return $ efs & saveCurrWS & setNewWS

moveToCell :: Row -> Column -> NewExcelFileState
moveToCell r c efs = efs & cellPos .~ (r, c)

rowApply = undefined

columnApply = undefined

addColumnValue = undefined

addRowValue = undefined

-- replaceColumnValue = undefined

-- replaceRowValue = undefined

insertColumn = undefined

insertRow = undefined

deleteColumn = undefined

deleteRow = undefined

setConstant = undefined

setConstantRow = undefined

setConstantCol = undefined

fillCells = undefined

-- readExcelFile :: String -> IO Xlsx
-- readExcelFile f = do
--   bs <- BL.readFile f
--   return $ toXlsx bs


-- writeExcelFile :: Xlsx -> FilePath -> IO ()
-- writeExcelFile x f = do
--   ct <- getPOSIXTime
--   BL.writeFile f $ fromXlsx ct x

