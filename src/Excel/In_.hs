{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}

module Excel.In_ where

-- 
--  Functions used for "In ___ [] End" parameter only
--

import Data.Text
import Control.Lens
import Data.Traversable
import GHC.Generics (Generic)

import Codec.Xlsx
import Codec.Xlsx.Lens
import Codec.Xlsx.Types


-- We want explicit new sheet creation in DSL
inSheet :: Text -> Traversable Xlsx Worksheet
inSheet = ixSheet

