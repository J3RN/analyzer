module Main (main) where

import Lib
import CSV

import Data.List (intercalate, sort)

import Options.Applicative

data Command
  = Paths FilePath PathsOptions
  | Callers FilePath CallersOptions
  | Callees FilePath CalleesOptions
  | Dependents FilePath DependentsOptions
  | Dependencies FilePath DependenciesOptions
  | Dot FilePath DotOptions

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

data DotOptions = DotOptions
  { outFile :: String
  , sinks :: [String]
  , sources :: [String]
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
    <> command "dot" (info dotCommand (progDesc "Output a visualization of the call graph in GraphViz DOT format"))
  ))

pathsCommand :: Parser Command
pathsCommand = Paths
               <$> argument str (metavar "INPUTFILE")
               <*> (PathsOptions
                    <$> argument str (metavar "SOURCE")
                    <*> argument str (metavar "SINK"))

callersCommand :: Parser Command
callersCommand = Callers
                 <$> argument str (metavar "INPUTFILE")
                 <*> (CallersOptions <$> argument str (metavar "CALLEE"))

calleesCommand :: Parser Command
calleesCommand = Callees
                 <$> argument str (metavar "INPUTFILE")
                 <*> (CalleesOptions <$> argument str (metavar "CALLER"))

dependentsCommand :: Parser Command
dependentsCommand = Dependents
                    <$> argument str (metavar "INPUTFILE")
                    <*> (DependentsOptions <$> argument str (metavar "DEPENDENCY"))

dependenciesCommand :: Parser Command
dependenciesCommand = Dependencies
                      <$> argument str (metavar "INPUTFILE")
                      <*> (DependenciesOptions <$> argument str (metavar "DEPENDENT"))

dotCommand :: Parser Command
dotCommand = Dot
             <$> argument str (metavar "INPUTFILE")
             <*> (DotOptions
                  <$> strOption (long "out"
                                 <> short 'o'
                                 <> metavar "FILE"
                                 <> value "out.dot"
                                 <> help "File to write the output to (defaults to 'out.dot')")
                  <*> many (strOption (long "sink"
                                       <> metavar "SINK"
                                       <> help "Terminal function, a leaf in the call tree.  Multiple sinks are ORed together."))
                  <*> many (strOption (long "source"
                                       <> metavar "SOURCE"
                                       <> help "Source function, root in the call tree.  Multiple sinks are ORed together.")))


processCommand :: Command -> IO ()
processCommand (Paths inputFile options) = do
  calls <- readCSV inputFile
  putStrLn $ intercalate "\n\n" $ map (intercalate "\n") $ dfsPaths calls (source options) (sink options)

processCommand (Callers inputFile options) = do
  calls <- readCSV inputFile
  putStrLn $ intercalate "\n" $ sort $ head $ dependents calls (callee options)

processCommand (Callees inputFile options) = do
  calls <- readCSV inputFile
  putStrLn $ intercalate "\n" $ sort $ head $ dependencies calls (caller options)

processCommand (Dependents inputFile options) = do
  calls <- readCSV inputFile
  putStrLn $ intercalate "\n\n" $ fmap (intercalate "\n" . sort) $ dependents calls (dependency options)

processCommand (Dependencies inputFile options) = do
  calls <- readCSV inputFile
  putStrLn $ intercalate "\n\n" $ fmap (intercalate "\n" . sort) $ dependencies calls (dependent options)

processCommand (Dot inputFile options) = do
  calls <- readCSV inputFile
  writeFile (outFile options) (dot calls (sources options) (sinks options))
  putStrLn $ "Output written to " <> (outFile options)

readCSV :: String -> IO [(String, String)]
readCSV inputFile = do
  contents <- readFile $ inputFile
  either (fail.show) pure $ CSV.parse inputFile contents
