module Main (main) where

import Lib

import System.Environment (getArgs)

import Text.Parsec
import Text.Parsec.String (Parser)

import Data.List (intercalate, uncons)

main :: IO ()
main = do
  args <- getArgs
  (command, rest) <- maybe (fail "Must specify a command") pure $ Data.List.uncons args
  processCommand command rest

processCommand :: String -> [String] -> IO ()
processCommand "paths" args = processPaths args
processCommand cmd _args = fail ("Unknown command: " <> cmd)

processPaths :: [String] -> IO ()
processPaths args = do
  (input, source, sink) <- maybe (fail "Required arguments: inputfile, source, sink") pure $ ensureArgs args
  contents <- readFile input
  mappings <- either (fail . show) pure $ runParser parseCSV () input contents
  putStrLn $ intercalate "\n\n" $ map (intercalate "\n") $ dfsPaths mappings source sink

ensureArgs :: [String] -> Maybe (String, String, String)
ensureArgs [input, source, sink] = Just (input, source, sink)
ensureArgs _ = Nothing

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
parseString = do
  _ <- char '"'
  s <- many1 (satisfy (/= '"'))
  _ <- char '"'
  return s


