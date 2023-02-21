module Main (main) where

import Lib
import CSV

import Data.List (intercalate)

import Options.Applicative

type FileName = String

data Command
  = Paths FileName PathsOptions
  | Callers FileName CallersOptions
  | Callees FileName CalleesOptions
  | Dependents FileName DependentsOptions
  | Dependencies FileName DependenciesOptions

data PathsOptions = PathsOptions
  { source :: String
  , sink :: String
  }

data CallersOptions = CallersOptions
  { callee :: String
  }

data CalleesOptions = CalleesOptions
  { caller :: String
  }

data DependentsOptions = DependentsOptions
  { dependency :: String
  }

data DependenciesOptions = DependenciesOptions
  { dependent :: String
  }

main :: IO ()
main = do
  cmd <- execParser opts
  processCommand cmd

opts :: ParserInfo Command
opts = info (analyzerCommand <**> helper) fullDesc

analyzerCommand :: Parser Command
analyzerCommand = (hsubparser
  ( command "paths" (info pathsCommand (progDesc "Show the call-graph paths from a source to a sink"))
    <> command "callers" (info callersCommand (progDesc "List the functions that call the specified function"))
    <> command "callees" (info calleesCommand (progDesc "List the functions that the specified function calls"))
    <> command "dependents" (info dependentsCommand (progDesc "List the functions that call the specified function, recursively"))
    <> command "dependencies" (info dependenciesCommand (progDesc "List the functions the specified function calls, recursively"))
  ))

pathsCommand :: Parser Command
pathsCommand = Paths
               <$> argument str (metavar "inputfile")
               <*> (PathsOptions
                    <$> argument str (metavar "source")
                    <*> argument str (metavar "sink"))

callersCommand :: Parser Command
callersCommand = Callers
                 <$> argument str (metavar "inputfile")
                 <*> (CallersOptions <$> argument str (metavar "callee"))

calleesCommand :: Parser Command
calleesCommand = Callees
                 <$> argument str (metavar "inputfile")
                 <*> (CalleesOptions <$> argument str (metavar "caller"))

dependentsCommand :: Parser Command
dependentsCommand = Dependents
                    <$> argument str (metavar "inputfile")
                    <*> (DependentsOptions <$> argument str (metavar "dependency"))

dependenciesCommand :: Parser Command
dependenciesCommand = Dependencies
                      <$> argument str (metavar "inputfile")
                      <*> (DependenciesOptions <$> argument str (metavar "dependent"))

processCommand :: Command -> IO ()
processCommand (Paths inputFile options) = do
  calls <- readCSV inputFile
  putStrLn $ intercalate "\n\n" $ map (intercalate "\n") $ dfsPaths calls (source options) (sink options)

processCommand (Callers inputFile options) = do
  calls <- readCSV inputFile
  putStrLn $ intercalate "\n" $ head $ dependents calls (callee options)

processCommand (Callees inputFile options) = do
  calls <- readCSV inputFile
  putStrLn $ intercalate "\n" $ head $ dependencies calls (caller options)

processCommand (Dependents inputFile options) = do
  calls <- readCSV inputFile
  putStrLn $ intercalate "\n\n" $ fmap (intercalate "\n") $ dependents calls (dependency options)

processCommand (Dependencies inputFile options) = do
  calls <- readCSV inputFile
  putStrLn $ intercalate "\n\n" $ fmap (intercalate "\n") $ dependencies calls (dependent options)

readCSV :: String -> IO [(String, String)]
readCSV inputFile = do
  contents <- readFile $ inputFile
  either (fail.show) pure $ CSV.parse inputFile contents
