{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Parser.Parser where

import Control.Monad (liftM)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

-- I suck at design so avert your eyes


data InArgs = Row | Column | Source | Destination deriving (Show)

-- data TopLevelDeclaration
--   = Source FilePath -- Change in future
--   | Destination FilePath -- Change in future
--   | Receipt Bool
--   deriving (Show)

data Location
  = CellStart
  | CellEnd
  | CellEmpty
  | Header [Text]
  | CellRange (Maybe Int) (Maybe Int)
  deriving (Show)

data CellNavigation = Axis Location

-- [
--    TopLevelDeclaration,
--    TopLevelDeclaration,
--    Function copy [
-- 	XLFunc "Copy",
-- 	XLFunc "FVIFA"
--    ] End
--    Do [
-- 	In Row [
-- 	  XLFunc "NewSheet",
-- 	  copy
-- 	] End
--    ] End
-- ]

data End = End deriving (Show)

data Gram a where
  Do :: [Gram a] -> End -> Gram a
  In :: InArgs -> [Gram a] -> End -> Gram a
  Function :: Text -> [Gram a] -> End -> Gram a
  XLFunc :: Text -> Gram a
  CustomXLFunc :: Text -> Gram a
  EmptyScope :: Gram a

cShowL :: Show a => [Gram a] -> String
cShowL xs = "[ " ++ go xs ++ " ]"
  where go [] = []
        go [y] = show y
        go (y:ys) = show y ++ ", " ++ go ys

instance (Show a) => Show (Gram a) where
  show (Do gs e) = "Do " ++ cShowL gs ++ " " ++ show e
  show (In a gs e) = "In " ++ show a ++ " " ++ cShowL gs ++ " " ++ show e
  show (Function t gs e) = "Function " ++ show t ++ " " ++ cShowL gs ++ " " ++ show e
  show (XLFunc t) = "XLFunc " ++ show t
  show (CustomXLFunc t) = "CustomXLFunc " ++ show t
  show (EmptyScope) = "EmptyScope"

fpretty :: String -> String
fpretty = go 0
  where
    go _ "" = ""
    go l ('[' : xs) = "[" ++ "\n" ++ replicate (l + 1) '\t' ++ go (l + 1) xs
    go l (',' : xs) = "," ++ "\n" ++ replicate l '\t' ++ go l xs
    -- go l (']' : _ : xs) = "\n" ++ replicate (l - 1) '\t' ++ "]" ++ "\n" ++ replicate (l - 1) '\t' ++ go (l - 1) xs
    go l (']' : xs) = "\n" ++ replicate (l - 1) '\t' ++ "]" ++ go (l - 1) xs
    go l (x : xs) = x : go l xs


type FParser = Parsec Void Text

parseRowCol :: FParser InArgs
parseRowCol = (Row <$ string' "Row") <|> (Column <$ string' "Column")

parseSourceDes :: FParser InArgs
parseSourceDes = (Source <$ string' "Source") <|> (Destination <$ string' "Destination")

parseEmptyScope :: FParser [Gram a]
parseEmptyScope = lookAhead (char ']') *> return [EmptyScope]

parseEmptyScope' :: FParser (Gram a)
parseEmptyScope' = liftM head parseEmptyScope

parseXLFunc :: FParser (Gram a)
parseXLFunc = some alphaNumChar >>= return . XLFunc . T.pack

parseCustomXLFunc :: FParser (Gram a)
parseCustomXLFunc = do
  char' 'c' >> space1
  f <- many alphaNumChar
  return . CustomXLFunc . T.pack $ f

parseInnerLoop :: [FParser (Gram a)] -> FParser [Gram a]
parseInnerLoop ps = liftM reverse $ loop []
  where
    loop xs = do
      space
      x <- choice ps
      case x of
        EmptyScope -> return xs
        _ -> loop (x : xs)

parseInnerIn :: FParser (Gram a)
parseInnerIn = do
  string' "In"
  space
  a <-
    between (char '(') (char ')') $
      choice
        [ parseRowCol,
          parseSourceDes
        ]
  space
  f <-
    between (char '[') (char ']') $
      choice
        [ parseEmptyScope,
          parseInnerLoop
            [ parseCustomXLFunc,
              parseXLFunc,
              parseEmptyScope'
            ]
        ]
  space
  string' "End"
  return $ In a f End

parseIn :: FParser (Gram a)
parseIn = do
  string' "In"
  space
  a <-
    between (char '(') (char ')') $
      choice
        [ parseRowCol,
          parseSourceDes
        ]
  space
  f <-
    between (char '[') (char ']') $
      choice
        [ parseEmptyScope,
          parseInnerLoop
            [ parseInnerIn,
              parseCustomXLFunc,
              parseXLFunc,
              parseEmptyScope'
            ]
        ]
  space
  string' "End"
  return $ In a f End

parseDo :: FParser (Gram a)
parseDo = do
  space
  string' "Do"
  space
  x <-
    between (char '[') (char ']') $
      choice
        [ parseEmptyScope,
          parseInnerLoop
            [ parseIn,
              parseCustomXLFunc,
              parseXLFunc,
              parseEmptyScope'
            ]
        ]
  space
  string' "End"
  space
  return $ Do x End

parseFunction :: FParser (Gram a)
parseFunction = do
  space
  string' "Function"
  space1
  n <- some alphaNumChar
  space
  f <-
    between (char '[') (char ']') $
      choice
        [ parseEmptyScope,
          parseInnerLoop
            [ parseIn,
              parseCustomXLFunc,
              parseXLFunc,
              parseEmptyScope'
            ]
        ]
  space
  string' "End"
  space
  return $ Function (T.pack n) f End

-- [TopLevelDeclaration, TopLevelDeclaration, Function copy [Copy, FVIFA] End, Do [In Row [NewSheet, Paste, copy], End] End]
parseDXL :: String -> Text -> Either (ParseErrorBundle Text Void) [Gram a]
parseDXL = runParser (manyTill (parseDo <|> parseFunction) eof)


eval = undefined

