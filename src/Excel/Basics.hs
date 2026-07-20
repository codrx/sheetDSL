{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes        #-}

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

import           Control.Lens
import           Control.Monad
import           Control.Monad.Except
import           Control.Applicative

import           Codec.Xlsx
import           Excel.Type


{-

  Semantics:

    update____ -> any function that modifies ExcelFileState 

      -> used for tracking more than anything else


    write_____ -> any function that modifies records defined in Xlsx library

      -> anything that changes records in xlsx library will directly effect the file


  Majority of functions will overwrite existing cell contents

-}

{------------------------------------------------------------

  Helpers 

-}

-- improve error messages

-- can be empty in the beginning
_getActiveSheet :: MonadError Text m => ExcelFileState -> m (SheetName, Worksheet)
_getActiveSheet x
  | Just as' <- x ^. activeSheet = pure as'
  | otherwise                    = throwError "NO SHEET SET AS ACTIVE."

_getActiveWS :: MonadError Text m => ExcelFileState -> m Worksheet
_getActiveWS = fmap snd . _getActiveSheet

_getCellPos :: ExcelFileState -> CellCoord
_getCellPos = flip (^.) cellPos

_getInactiveSheet :: MonadError Text m => SheetName -> ExcelFileState -> m (SheetName, Worksheet)
_getInactiveSheet s x
  | Just ws' <- x ^? xlsx . ixSheet s = pure (s, ws')
  | otherwise                         = throwError "SHEET NOT FOUND."

_getInactiveWS :: MonadError Text m => SheetName -> ExcelFileState -> m Worksheet
_getInactiveWS s = fmap snd . _getInactiveSheet s

{------------------------------------------------------------

  Something 

-}

copy = undefined

paste = undefined

newSheet :: SheetName -> NewExcelFileState 
newSheet s = xlsx . atSheet s ?~ def

setActiveSheet :: SheetName -> ENewExcelFileState
setActiveSheet s x = do
  a <- _getActiveSheet x
  i <- _getInactiveSheet s x
  return $ update a i x

    where update (as, aw) (is, iw) x' = x' & save & setA

            where save = xlsx . ixSheet as .~ aw
                  setA = activeSheet ?~ (is, iw)

{-

  CellRef "cr"= user friendly position e.g. A1, B2

  CellCoord r c = machine friendly position e.g CellRef B3 <-> CellCoord 3 2

-}

{------------------------------------------------------------

  Moving 

-}

updateCellPos :: CellCoord -> NewExcelFileState
updateCellPos = (.~) cellPos

-- moveToCell (row = 3, coloumn = B)
moveToCell :: Row -> Column -> NewExcelFileState
moveToCell r c x = 
  let r' = row2coord r
      c' = col2coord c 
    in updateCellPos (r', c') x



moveUp :: NewExcelFileState
moveUp = undefined

{------------------------------------------------------------

  Modifying wsCell in Worksheet

-}

-- Taken from: Codec.Xlsx.Types.Common
-- Unwrap a Coord into an abstract Int coordinate
_unRowCoord :: RowCoord -> RowIndex
_unRowCoord (RowAbs i) = i
_unRowCoord (RowRel i) = i

-- Taken from: Codec.Xlsx.Types.Common
-- Unwrap a Coord into an abstract Int coordinate
_unColumnCoord :: ColumnCoord -> ColumnIndex
_unColumnCoord (ColumnAbs i) = i
_unColumnCoord (ColumnRel i) = i

-- !TODO: define Prism to handle Nothing case - _Just skips nothing
writeToWS :: WSContent -> NewExcelFileState
writeToWS (WSCells f) x = x & activeSheet . _Just . _2 . wsCells %~ f
-- !TODO: Other value is WSColumn - idk rn what to do with it or why it's there
-- should not be hit -- change 
writeToWS _ _ = undefined

writeCellToWS :: (CellCoord, Cell) -> NewExcelFileState
writeCellToWS (cc, c) x = 
  let coordAsIndex = bimap _unRowCoord _unColumnCoord cc
    in writeToWS (WSCells (at coordAsIndex ?~ c)) x

-- write to Cell record in xlsx
writeToCell :: CellContent -> Cell -> Cell
writeToCell (CCStyle cs) = (?~) cellStyle cs
writeToCell (CCValue cv) = (?~) cellValue cv
writeToCell (CCComment cm) = (?~) cellComment cm
writeToCell (CCFormula cf) = (?~) cellFormula cf


{------------------------------------------------------------

  Cell Insertion

-}

insertFormula :: a -> NewExcelFileState
insertFormula = undefined

-- not related to xlsx CellRef
insertCellRef :: a -> NewExcelFileState
insertCellRef = undefined

insertCellValue :: ValidCellValue a => a -> NewExcelFileState
insertCellValue v x = 
  let tc = toCellValue v
      -- !TODO: using def for Cell for now
      wc = writeToCell tc def
      cc = _getCellPos x
    in writeCellToWS (cc, wc) x

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


