-- Admittedly, yes, I probably should just use a CSV library
module CSV (parse) where

import Text.Parsec (between, char, many1, newline, runParser, sepEndBy, satisfy, string, ParseError)
import Text.Parsec.String (Parser)

parse :: String -> String -> Either ParseError [(String, String)]
parse inputFile contents =
  runParser parseCSV () inputFile contents

parseCSV :: Parser [(String, String)]
parseCSV = do
  _ <- string "source,target\n"
  sepEndBy parsePair newline

parsePair :: Parser (String, String)
parsePair = do
  source <- parseString
  _ <- char ','
  sink <- parseString
  return (source, sink)

parseString :: Parser String
parseString = between (char '"') (char '"') (many1 (satisfy (/= '"')))
