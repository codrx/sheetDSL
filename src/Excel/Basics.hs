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


import           Data.Maybe (fromJust)
import           Control.Lens
import           Codec.Xlsx
import           Excel.Type


{-

  Semantics:

    update____ -> any function that modifies ExcelFileState 

      -> used for tracking more than anything else


    write_____ -> any function that modifies records defined in Xlsx library

      -> anything that changes records in lsx library will directly effect the file


  Majority of functions will overwrite existing cell contents

  - may need to wrap accessors in newtype

-}

_getAS :: ExcelFileState -> (SheetName, Worksheet)
_getAS = fromJust . flip (^.) activeSheet

_getWS :: ExcelFileState -> Worksheet
_getWS = snd . _getAS

_getCellPos :: ExcelFileState -> CellCoord
_getCellPos = flip (^.) cellPos

copy = undefined

paste = undefined

newSheet :: SheetName -> NewExcelFileState 
newSheet sheetName = xlsx . atSheet sheetName ?~ def

setActiveSheet :: SheetName -> MNewExcelFileState
setActiveSheet = updateActiveSheet

-- improve | looks ugly
updateActiveSheet :: SheetName -> MNewExcelFileState
updateActiveSheet sheetName efs = do 
  newWS <- efs ^? xlsx . ixSheet sheetName
  let (sn, ws)   = _getAS efs
      saveCurrWS = xlsx . ixSheet sn .~ ws
      setNewWS   = activeSheet ?~ (sheetName, newWS)
  return $ efs & saveCurrWS & setNewWS

{-

  CellRef "cr"= user friendly position e.g. A1, B2

  CellCoord r c = machine friendly position e.g CellRef B3 <-> CellCoord 3 2

-}

{------------------------------------------------------------

  Moving 

-}

updateCellPos :: CellCoord -> NewExcelFileState
updateCellPos c = flip (&) (cellPos .~ c)

-- moveToCell (row = 3, coloumn = B)
moveToCell :: Row -> Column -> NewExcelFileState
moveToCell r c efs = 
  let r' = row2coord r
      c' = col2coord c 
    in updateCellPos (r', c') efs

{------------------------------------------------------------

  Modifying wsCell in Worksheet

-}

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

writeToWS :: WSContent -> NewExcelFileState
writeToWS (WSCells f) efs = efs & activeSheet . _Just . _2 . wsCells %~ f
-- not hit
writeToWS _ _ = undefined

writeCellToWS ::(CellCoord, Cell) -> NewExcelFileState
writeCellToWS (cc, c) efs = 
  let coordAsIndex = bimap unRowCoord unColumnCoord cc
    in writeToWS (WSCells (at coordAsIndex ?~ c)) efs

-- write to Cell record in xlsx
writeToCell :: CellContent -> Cell -> Cell
writeToCell (CCStyle cs) = flip (&) (cellStyle ?~ cs)
writeToCell (CCValue cv) = flip (&) (cellValue ?~ cv)
writeToCell (CCComment cm) = flip (&) (cellComment ?~ cm)
writeToCell (CCFormula cf) = flip (&) (cellFormula ?~ cf)


{------------------------------------------------------------

  Cell Insertion

-}

insertFormula :: a -> ENewExcelFileState
insertFormula = undefined

-- not related to xlsx CellRef
insertCellRef :: a -> ENewExcelFileState
insertCellRef = undefined

insertCellValue :: ValidCellValue a => a -> NewExcelFileState
insertCellValue v efs = 
  let tc = toCellValue v
      -- using def for Cell for now
      wc = writeToCell tc def
      cc = _getCellPos efs
    in writeCellToWS (cc, wc) efs

insertValue :: ValidCellValue a => a -> NewExcelFileState
insertValue = insertCellValue

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



-- To be moved into separate folder and file
-- or changed once i understand xlsx lib


-- need to add constraint

-- sumFormula :: GoesInCell a => (a, a) -> Formula
-- sumFormula (b, e)= 
--   let 
--     in ExcelFormula $ "=SUM" ++ "(" ++ b ++ "," ++ e ++ ")"


-- averageFormula = undefined


