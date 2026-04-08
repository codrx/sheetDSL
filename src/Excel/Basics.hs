{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}

module Excel.Basics
  ( newSheet
  , setActiveSheet
  , moveToCell
  , insertValue
  , setAbsRef
  , setAbsRefCol
  , setAbsRefRow
  , setConstant
  , setConstantRow
  , setConstantCol
  , rowApply
  , columnApply
  , fillCells
  ) where


import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock.POSIX
import qualified Data.ByteString.Lazy as BL
import           Data.Bifunctor (bimap)

import           Control.Lens


import           Codec.Xlsx
import           Codec.Xlsx.Types
import           Codec.Xlsx.Types.Common

import           Excel.Type


{-

  Majority of functions overwrite existing cell contents

  - may need to wrap accessors in newtype

-}


copy = undefined

_getCellData = undefined

paste = undefined

newSheet :: SheetName -> NewExcelFileState 
newSheet sheetName = xlsx . atSheet sheetName ?~ def

-- improve | looks ugly
setActiveSheet :: SheetName -> MNewExcelFileState
setActiveSheet sheetName efs = do 
  newWS <- efs ^? xlsx . ixSheet sheetName
  let (sn, ws)   = efs ^. activeSheet
      saveCurrWS = xlsx . ixSheet sn .~ ws
      setNewWS   = activeSheet .~ (sheetName, newWS)
  return $ efs & saveCurrWS & setNewWS

updateCellPos :: CellCoord -> NewExcelFileState
updateCellPos c efs = efs & cellPos .~ c


-- Taken from: Codec.Xlsx.Types.Common
-- | Unwrap a Coord into an abstract Int coordinate
unRowCoord :: RowCoord -> RowIndex
unRowCoord (RowAbs i) = i
unRowCoord (RowRel i) = i

-- Taken from: Codec.Xlsx.Types.Common
-- | Unwrap a Coord into an abstract Int coordinate
unColumnCoord :: ColumnCoord -> ColumnIndex
unColumnCoord (ColumnAbs i) = i
unColumnCoord (ColumnRel i) = i

writeCellToWS ::[(CellCoord, Cell)] -> NewExcelFileState
writeCellToWS [] efs = efs
writeCellToWS cs efs = undefined
  -- let coordToIndex = bimap unRowCoord unColumnCoord
  --   in undefined
{-# INLINE writeCellToWS #-}

moveToCell :: Row -> Column -> NewExcelFileState
moveToCell r c efs = 
  let r' = row2coord r
      c' = col2coord c 
    in updateCellPos (r', c') efs



insertValue :: GoesInCell a => a -> MNewExcelFileState
insertValue = undefined

rowApply = undefined

columnApply = undefined

-- addColumnValue = undefined

-- addRowValue = undefined

-- replaceColumnValue = undefined

-- replaceRowValue = undefined

-- insertColumn = undefined

-- insertRow = undefined

-- deleteColumn = undefined

-- deleteRow = undefined

setConstant = setAbsRef
setAbsRef = undefined

setConstantRow = setAbsRefRow
setAbsRefRow = undefined


setConstantCol = setAbsRefCol
setAbsRefCol = undefined


-- uses row & col apply
fillCells :: NewExcelFileState
fillCells = undefined

-- readExcelFile :: String -> IO Xlsx
-- readExcelFile f = do
--   bs <- BL.readFile f
--   return $ toXlsx bs


-- writeExcelFile :: Xlsx -> FilePath -> IO ()
-- writeExcelFile x f = do
--   ct <- getPOSIXTime
--   BL.writeFile f $ fromXlsx ct x

