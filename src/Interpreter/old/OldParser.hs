{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Parser.Old.OldParser 
  ( parseDXL
  , fpretty
  , Gram
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Data.Functor (($>))
-- import System.FilePath (isValid, takeExtension)
import Text.Megaparsec
import Text.Megaparsec.Char

-- I suck at design so avert your eyes
--
-- Will improve after working product completed


type FParser = Parsec Void Text

type Var = Text

data InArgs = Row | Column | Source | Destination deriving (Show)

data Location
  = CellStart
  | CellEnd
  | CellEmpty
  | Header [Text]
  | CellRange (Maybe Int) (Maybe Int)
  deriving (Show)

data End = End deriving (Show)


type FuncArgs = Maybe [(Var, Text)]


data Gram a where
  -- Settings :: TopLevelDeclaration -> Gram TopLevelDeclaration
  Do :: [Gram a] -> End -> Gram a
  In :: InArgs -> [Gram a] -> End -> Gram a
  Function :: Text -> FuncArgs -> [Gram a] -> End -> Gram a
  XLFunc :: Text -> FuncArgs -> Gram a
  CustomXLFunc :: Text -> FuncArgs -> Gram a
  EmptyScope :: Gram a


cShowL :: (Show a) => [Gram a] -> String
cShowL xs = "[ " ++ go xs ++ " ]"
  where
    go [] = []
    go [y] = show y
    go (y : ys) = show y ++ ", " ++ go ys

instance (Show a) => Show (Gram a) where
  show (Do gs e) = "Do " ++ cShowL gs ++ " " ++ show e
  show (In a gs e) = "In " ++ show a ++ " " ++ cShowL gs ++ " " ++ show e
  show (Function t as gs e) = "Function " ++ show t ++ " " ++ show as ++ " " ++ cShowL gs ++ " " ++ show e
  show (XLFunc t as) = "XLFunc " ++ show t ++ " " ++ show as 
  show (CustomXLFunc t as) = "CustomXLFunc " ++ show t  ++ " " ++ show as
  show (EmptyScope) = "EmptyScope"


fpretty :: String -> String
fpretty = go 0
  where
    go _ "" = ""
    go l ('[' : xs) = "[" ++ "\n" ++ replicate (l + 1) '\t' ++ go (l + 1) xs
    go l (',' : xs) = "," ++ "\n" ++ replicate l '\t' ++ go l xs
    go l (']' : xs) = "\n" ++ replicate (l - 1) '\t' ++ "]" ++ go (l - 1) xs
    go l (x : xs) = x : go l xs



parseVar :: FParser (Var, Text)
parseVar = do
  space
  var <- some alphaNumChar
  char '='
  val <- some alphaNumChar
  space
  return (T.pack var, T.pack val)


parseFuncArgs :: FParser FuncArgs
parseFuncArgs = try $ do
  char '('
  xs <- many (try (parseVar <* char ',') <|> parseVar)
  char ')'
  return $ case null xs of
    False -> Just xs
    _     -> Nothing

  <|> return Nothing


parseRowCol :: FParser InArgs
parseRowCol = (Row <$ string' "Row") <|> (Column <$ string' "Column")

parseSourceDes :: FParser InArgs
parseSourceDes = (Source <$ string' "Source") <|> (Destination <$ string' "Destination")

parseSheet :: FParser InArgs
parseSheet = undefined

parseEmptyScope :: FParser [Gram a]
parseEmptyScope = lookAhead (char ']') $> [EmptyScope]

parseEmptyScope' :: FParser (Gram a)
parseEmptyScope' = fmap head parseEmptyScope

parseXLFunc :: FParser (Gram a)
parseXLFunc = do 
  f <- some alphaNumChar
  space
  r <- parseFuncArgs
  return $ XLFunc (T.pack f) r

parseCustomXLFunc :: FParser (Gram a)
parseCustomXLFunc = do
  char' 'c'
  space1
  f <- many alphaNumChar
  space
  r <- parseFuncArgs
  return $ CustomXLFunc (T.pack f) r

parseInnerLoop :: [FParser (Gram a)] -> FParser [Gram a]
parseInnerLoop ps = reverse <$> loop []
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
  space1
  a <- (space >> choice [parseRowCol, parseSourceDes, parseSheet]) <* space
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
  space1
  a <- (space >> choice [ parseRowCol, parseSourceDes ]) <* space
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
  x <- parseFuncArgs
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
  return $ Function (T.pack n) x f End

-- [TopLevelDeclaration, TopLevelDeclaration, Function copy [Copy, FVIFA] End, Do [In Row [NewSheet, Paste, copy], End] End]
parseDXL :: String -> Text -> Either (ParseErrorBundle Text Void) [Gram a]
parseDXL = runParser $ do 
  f <- many parseFunction
  r <- parseDo
  return $ reverse (r : f)

eval = undefined
