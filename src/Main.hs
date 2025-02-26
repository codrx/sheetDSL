-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Data.Time.Clock.POSIX
import qualified Data.Text.IO as T (readFile)
import qualified Data.ByteString.Lazy as L
import Codec.Xlsx
import Control.Lens
import System.FilePath (isValid, takeExtension)


import Excel.Basics
-- import Excel.Finance
-- import Excel.Accounting
-- import Excel.Styling
import Parser.Parser (parseDXL, fpretty, Gram) 


writeExcelFile :: Xlsx -> FilePath -> IO ()
writeExcelFile xlsx fp = do
  ct <- getPOSIXTime
  L.writeFile fp $ fromXlsx ct xlsx

-- test :: FilePath -> IO ()
-- test fp = do
--   ct <- getPOSIXTime
--   bs <- L.readFile fp
--   seq (L.length bs) (return ())
--   L.putStr $ show $ length $ bs ^. xlSheets
--   let ns = newSheet $ toXlsx bs
--       xlsx = def & ns ?~ def
--   L.writeFile fp $ fromXlsx ct xlsx

-- test = withFile "example.xlsx" ReadMode $ \h -> do
--     bs <- L.hGetContents h
--     undefined

printParseResult :: FilePath -> IO ()
printParseResult fp = do
    content <- T.readFile fp
    case parseDXL fp content of
        Left err -> putStrLn $ "Parse error: " ++ show err
        Right values -> putStrLn . fpretty . show $ (values :: [Gram Text])
        -- Right values -> print (values :: [Gram Text])

main :: IO ()
main = printParseResult "test.dxl"
