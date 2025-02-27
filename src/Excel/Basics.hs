{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}

module Excel.Basics where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Lens
import Data.Traversable
import GHC.Generics (Generic)

import Codec.Xlsx
import Codec.Xlsx.Lens
import Codec.Xlsx.Types

import qualified Data.ByteString.Lazy as L


-- Single sheet for now
-- newSheet :: Text -> Lens' Xlsx (Maybe Worksheet)
-- newSheet = atSheet

copy = undefined

paste = undefined

-- getColumnValue = undefined

-- getRowValue = undefined

-- addColumnValue = undefined

-- addRowValue = undefined

-- replaceColumnValue = undefined

-- replaceRowValue = undefined

-- insertColumn = undefined

-- insertRow = undefined

-- deleteColumn = undefined

-- deleteRow = undefined
