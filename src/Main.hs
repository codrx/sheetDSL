-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Excel.Basics
-- import Excel.Finance
-- import Excel.Accounting
-- import Excel.Styling
import Data.Text (Text)
import qualified Data.Text.IO as T (readFile)
import System.FilePath (isValid, takeExtension)
import Parser.Parser (parseDXL, fpretty, Gram) 


printParseResult :: FilePath -> IO ()
printParseResult fp = do
    content <- T.readFile fp
    case parseDXL fp content of
        Left err -> putStrLn $ "Parse error: " ++ show err
        Right values -> putStrLn . fpretty . show $ (values :: [Gram Text])
        -- Right values -> print (values :: [Gram Text])

main :: IO ()
main = printParseResult "test.dxl"
