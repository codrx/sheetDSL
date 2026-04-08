{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Interpreter.Dispatch where


import           Prelude hiding (lookup)

import           Data.Text (Text)

import           Data.Map
import qualified Data.Map as M

import           Excel.Basics

{-
  TODO minoris:

    - Proxy dispatch table?

-}
-- import           Data.Proxy (Proxy)


data ExecFunc where
  ExecFunc :: forall f. f -> ExecFunc 

getFunc :: Text -> Maybe ExecFunc
getFunc = flip lookup dispatchTable

checkFunc :: Text -> Bool
checkFunc = flip member dispatchTable

{-
    Don't need to check for correct arguments as file
     write will happen after all functions are executed
-}
dispatchTable :: Map Text ExecFunc
dispatchTable = M.fromList
  [ ("NewSheet", ExecFunc newSheet) 
  , ("SetCurrentSheet", ExecFunc setActiveSheet)
  , ("MoveToCell", ExecFunc moveToCell)
  , ("InsertValue", ExecFunc insertValue)
  ]