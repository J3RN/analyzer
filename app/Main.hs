module Main (main) where

import Lib

import System.Environment (getArgs)

import Text.Parsec
import Text.Parsec.String (Parser)

import Data.List (intercalate, uncons)

------------- TODO: Replace custom arg parsing with a nice library -------------

main :: IO ()
main = do
  args <- getArgs
  (command, rest) <- maybe (fail "Must specify a command") pure $ Data.List.uncons args
  (inputFile, rest') <- maybe (fail "Must specify a data file") pure $ Data.List.uncons rest
  contents <- readFile inputFile
  mappings <- either (fail . show) pure $ runParser parseCSV () inputFile contents
  processCommand mappings command rest'

processCommand :: Mappings -> String -> [String] -> IO ()
processCommand mappings  "paths"   args  = processPaths mappings args
processCommand mappings  "callers" args  = processCallers mappings args
processCommand mappings  "callees" args  = processCallees mappings args
processCommand _mappings cmd       _args = fail ("Unknown command: " <> cmd)

processPaths :: Mappings -> [String] -> IO ()
processPaths mappings args = do
  (source, sink) <- maybe (fail "Required arguments: inputfile, source, sink") pure $ ensurePathsArgs args
  putStrLn $ intercalate "\n\n" $ map (intercalate "\n") $ dfsPaths mappings source sink

ensurePathsArgs :: [String] -> Maybe (String, String)
ensurePathsArgs [source, sink] = Just (source, sink)
ensurePathsArgs _              = Nothing

processCallers :: Mappings -> [String] -> IO ()
processCallers mappings args = do
  callee <- maybe (fail "Required arguments for callers: source") pure $ ensureCallersArgs args
  putStrLn $ intercalate "\n" $ callers mappings callee

ensureCallersArgs :: [String] -> Maybe String
ensureCallersArgs [callee] = Just callee
ensureCallersArgs _        = Nothing

processCallees :: Mappings -> [String] -> IO ()
processCallees mappings args = do
  caller <- maybe (fail "Required arguments for callees: source") pure $ ensureCalleesArgs args
  putStrLn $ intercalate "\n" $ callees mappings caller

ensureCalleesArgs :: [String] -> Maybe String
ensureCalleesArgs [caller] = Just caller
ensureCalleesArgs _        = Nothing

--------------------------------------------------------------------------------

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


