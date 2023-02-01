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
  calls <- either (fail . show) pure $ runParser parseCSV () inputFile contents
  processCommand calls command rest'

processCommand :: Calls -> String -> [String] -> IO ()
processCommand calls  "paths"        args  = processPaths calls args
processCommand calls  "callers"      args  = processCallers calls args
processCommand calls  "callees"      args  = processCallees calls args
processCommand calls  "dependencies" args  = processDependencies calls args
processCommand calls  "dependents"   args  = processDependents calls args
processCommand _calls cmd            _args = fail ("Unknown command: " <> cmd)

processPaths :: Calls -> [String] -> IO ()
processPaths calls args = do
  (source, sink) <- maybe (fail "Required arguments: inputfile, source, sink") pure $ ensurePathsArgs args
  putStrLn $ intercalate "\n\n" $ map (intercalate "\n") $ dfsPaths calls source sink

ensurePathsArgs :: [String] -> Maybe (String, String)
ensurePathsArgs [source, sink] = Just (source, sink)
ensurePathsArgs _              = Nothing

processCallers :: Calls -> [String] -> IO ()
processCallers calls args = do
  callee <- maybe (fail "Required arguments for callers: source") pure $ ensureCallersArgs args
  putStrLn $ intercalate "\n" $ callers calls callee

ensureCallersArgs :: [String] -> Maybe String
ensureCallersArgs [callee] = Just callee
ensureCallersArgs _        = Nothing

processCallees :: Calls -> [String] -> IO ()
processCallees calls args = do
  caller <- maybe (fail "Required arguments for callees: source") pure $ ensureCalleesArgs args
  putStrLn $ intercalate "\n" $ callees calls caller

ensureCalleesArgs :: [String] -> Maybe String
ensureCalleesArgs [caller] = Just caller
ensureCalleesArgs _        = Nothing

processDependencies :: Calls -> [String] -> IO ()
processDependencies calls args = do
  source <- maybe (fail "dependencies command requires a single 'source' argument") pure $ ensureDependenciesArgs args
  putStrLn $ intercalate "\n" $ dependencies calls source

ensureDependenciesArgs :: [String] -> Maybe String
ensureDependenciesArgs [source] = Just source
ensureDependenciesArgs _        = Nothing

processDependents :: Calls -> [String] -> IO ()
processDependents calls args = do
  source <- maybe (fail "dependents command requires a single 'source' argument") pure $ ensureDependentsArgs args
  putStrLn $ intercalate "\n" $ dependents calls source

ensureDependentsArgs :: [String] -> Maybe String
ensureDependentsArgs [source] = Just source
ensureDependentsArgs _        = Nothing

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


