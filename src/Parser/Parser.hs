{-# LANGUAGE OverloadedStrings #-}

module Parser.Parser where

import Parser.Types

import Data.Text (Text)
import Data.Void (Void)
import qualified Data.Text as T

import Data.Functor (($>))

import Text.Megaparsec 
import Text.Megaparsec.Char



parseDXL :: String -> Text -> Either (ParseErrorBundle Text Void) [Stmt a]
parseDXL = runParser $ do 
  src  <- parseSource
  dest <- parseDestination
  rest <- parseDo
  return $ [src] ++ [dest] ++ [rest]


parseSource :: Parser (Stmt a)
parseSource = do 
  stmt <- (string' "Source" <|> string' "SOURCE") >> return Src
  _ <- char ':'
  space1
  path <- between (char '"') (char '"') $
           some (alphaNumChar <|> oneOf ("./_-" :: String))
  return $ stmt (T.pack path)


parseDestination :: Parser (Stmt a)
parseDestination = do 
  stmt <- (string' "Destination" <|> string' "DESTINATION") >> return Dest
  _ <- char ':'
  space1
  path <- between (char '"') (char '"') $
            some (alphaNumChar <|> oneOf ("./_-" :: String))
  return $ stmt (T.pack path)


parseInnerLoop :: [Parser (Stmt a)] -> Parser [Stmt a]
parseInnerLoop ps = reverse <$> loop []
  where
    loop xs = do
      space
      x <- choice ps
      case x of
        EmptyScope -> return xs
        _ -> loop (x : xs)

parseEmptyScope :: Parser [Stmt a]
parseEmptyScope = lookAhead (char ']') $> [EmptyScope]

parseEmptyScope' :: Parser (Stmt a)
parseEmptyScope' = fmap head parseEmptyScope

parseAssignment :: Parser (Var, Text)
parseAssignment = do
  space
  var <- some alphaNumChar
  _ <- char '='
  val <- some alphaNumChar
  space
  return (T.pack var, T.pack val)

parseParameter :: Parser FArgs
parseParameter = optional $ do 
  _ <- char '(' *> space
  args <- parseAssignment `sepBy` (char ',' *> space)
  _ <- char ')' *> space
  return args

parseBuiltinFunction :: Parser (Stmt a)
parseBuiltinFunction = do
  name <- some alphaNumChar
  space
  args <- parseParameter
  return $ BFunc (T.pack name) args


parseDo :: Parser (Stmt a) 
parseDo = do
  space
  stmt <- (string' "Do" <|> string' "DO") >> return Do
  space
  block <- 
    between (char '[') (char ']') $
      choice 
        [ parseEmptyScope, 
          parseInnerLoop 
          [ parseBuiltinFunction
          , parseEmptyScope'
          ]
        ]
  space
  stmt' <- (string' "End" <|> string' "END") >> return End
  space
  return $ stmt block stmt'