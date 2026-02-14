-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Main where

-- import Excel.Basics
-- import Excel.Finance
-- import Excel.Accounting
-- import Excel.Styling

-- main :: IO ()
-- main = writeExcelFile test "./Book1.xlsx"

-- printParseResult :: FilePath -> IO ()
-- printParseResult fp = do
--     content <- TIO.readFile fp
--     case parseDXL fp content of
--         Left err -> putStrLn $ "Parse error: " ++ show err
--         Right values -> putStrLn . fpretty . show $ (values :: [Gram Text])
--         -- Right values -> print (values :: [Gram Text])

-- main :: IO ()
-- main = printParseResult "test.dxl"

main :: IO ()
main = undefined