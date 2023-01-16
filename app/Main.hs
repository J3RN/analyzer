module Main (main) where

import Lib
import System.Environment (getArgs)

import Text.Parsec
import Text.Parsec.String (Parser)

main :: IO ()
main = do
  args <- getArgs
  (input, source, sink) <- maybe (fail "Required arguments: inputfile, source, sink") pure $ ensureArgs args
  contents <- readFile input
  mappings <- either (fail . show) pure $ runParser parseCSV () input contents
  mapM_ putStrLn $ path mappings source sink

ensureArgs :: [String] -> Maybe (String, String, String)
ensureArgs [input, source, sink] = Just (input, source, sink)
ensureArgs _ = Nothing

parseCSV :: Parser [(String, String)]
parseCSV = many parsePair

parsePair :: Parser (String, String)
parsePair = do
  source <- parseString
  _ <- char ','
  sink <- parseString
  return (source, sink)

parseString :: Parser String
parseString = do
  _ <- char '"'
  s <- many1 (satisfy (/= '"'))
  _ <- char '"'
  return s


