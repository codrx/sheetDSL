{-# LANGUAGE OverloadedStrings #-}

module Interpreter.Old.Parser (parseFromFile) where

import           Interpreter.Old.Type hiding (FilePath)

import           Data.Void (Void)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import           Text.Megaparsec 
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


{-

  TODO minoris: 
    - label parser <?>
    - improve filepath parsing | looks ugly

-}


type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

signed :: Num a => Parser a -> Parser a
signed = L.signed spaceConsumer

_assignEq, _assignCol :: Parser ()
_assignEq  = skipSome $ symbol "="
_assignCol = skipSome $ symbol ":"

_newline :: Parser ()
_newline = skipSome eol

_float :: Parser Double
_float = signed $ lexeme L.float

_integer :: Parser Int
_integer = signed $ lexeme L.decimal

_bool :: Parser Bool
_bool = (True <$ string' "True") <|> (False <$ string' "False")

_text :: Parser Text
_text = T.pack <$> str
  where str = char '"' *> someTill alphaNumChar (char '"')

_text' :: [Parser ()] -> Parser Text
_text' end = T.pack <$> str
  where str = someTill alphaNumChar (lookAhead $ choice end)


parseVarValue :: Parser VarValueTy
parseVarValue = choice $ try <$>
  [ FractionalTy <$> _float
  , IntegerTy    <$> _integer
  , BooleanTy    <$> _bool
  , StringTy     <$> _text
  ]

parseVariable, parseFuncName :: Parser Identifier
parseVariable = Variable <$> _text' [hspace1, _assignEq]
parseFuncName = FuncName <$> _text' [space1, _newline] 

-- should this be here?
showExt :: FileType -> String
showExt Excel = "xlsx"
showExt CSV   = "csv"

parseFileName :: Parser FileType -> Parser Identifier
parseFileName ext = between (char '"') (char '"') $ do
  n <- some alphaNumChar
  char '.'
  e <- ext
  return $ File e (T.pack (n <> "." <> showExt e))

-- looks ugly
parseArguments :: Parser Argument
parseArguments =
  fmap emptyToNothing $
    optional $ between (symbol "(") (symbol ")") (arg `sepBy` comma)
  where
    emptyToNothing (Just []) = Nothing
    emptyToNothing x         = x

    comma = symbol ","
    arg = do
      var <- parseVariable
      _assignEq
      val <- parseVarValue
      return (var, val)

parseRow, parseCol :: Parser Range
parseRow = string "row" *> _assignEq >> Row <$> _text
parseCol = string "column" *> _assignEq >> Column <$> _text

parseSelection :: Parser Range
parseSelection = between (symbol "(") (symbol ")") p
  where p = liftA2 Selection (parseRow <* symbol ",") parseCol

parseRange :: Parser Range
parseRange = liftA2 Selection (parseSelection <* hspace1) parseSelection

parseReadFrom :: Parser DSLGrammar
parseReadFrom = string' "ReadFrom" *> _assignCol >> ReadFrom <$> p
  where p = parseFileName validExt 
        validExt = CSV <$ string "csv"

parseWriteTo :: Parser DSLGrammar
parseWriteTo = string' "WriteTo" *> _assignCol >> WriteTo <$> p
  where p = parseFileName validExt
        validExt = Excel <$ string "xlsx"

parseFunction :: Parser DSLGrammar
parseFunction = liftA2 Function (parseFuncName <* space) parseArguments 

parseWithinBlock :: Parser DSLGrammar
parseWithinBlock = do
  symbol "Do"
  s <- parseRange
  f <- do
      symbol "["
      b <- optional $ some (parseFunction <* space1)
      symbol "]"
      return b
  symbol "End"
  return $ WithinBlock s f

parseDoBlock :: Parser DSLGrammar
parseDoBlock = do
  symbol "Do"
  s <- do
        symbol "["
        b <-  optional 
            . some $
                      try parseWithinBlock
                  <|> parseFunction
        symbol "]"
        return b
  symbol "End"
  return $ DoBlock s


parseFile :: Parser [DSLGrammar]
parseFile = do
  r <- parseReadFrom 
  space1
  w <- parseWriteTo
  space1
  d <- parseDoBlock
  return [r, w, d]

parseFromFile :: FilePath -> IO (Either Text [DSLGrammar])
parseFromFile file = do
  input <- TIO.readFile file
  return $ case runParser (parseFile <* eof) file input of
            Left err -> Left  $ T.pack $ errorBundlePretty err
            Right xs -> Right xs