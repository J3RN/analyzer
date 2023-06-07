module Lib
    ( dfsPaths
    , dependencies
    , dependents
    , dot
    , Call
    ) where

import Data.Bifunctor (bimap)
import Data.Function ((&))
import Data.List (findIndices, intercalate, partition)
import Data.Map()
import qualified Data.Map as Map
import Data.Set()
import qualified Data.Set as Set

type Path = [String]
type Call = (String, String)

dfsPaths :: [Call] -> String -> String -> [Path]
dfsPaths calls source sink =
  let (callsFromSource, otherCalls) = partition ((== source) . caller) calls
      (fromSourceToSink, unfinishedFromSource) = partition ((== sink) . callee) callsFromSource
      longerPaths = unfinishedFromSource & map (finishedPaths otherCalls sink) & concat
  in (map callToPath fromSourceToSink) ++ longerPaths

callToPath :: Call -> Path
callToPath (cr, ce) = [cr, ce]

finishedPaths :: [Call] -> String -> Call -> [Path]
finishedPaths calls sink (cr, ce) = map (cr:) (dfsPaths calls ce sink)

dependencies :: [Call] -> String -> [[String]]
dependencies calls source =
  let (childCalls, otherCalls) = partition ((== source) . caller) calls
      callees = map callee childCalls
  in case childCalls of
       [] -> []
       _  -> callees:(callees & map (dependencies otherCalls) & concat)

dependents :: [Call] -> String -> [[String]]
dependents calls source =
  let (callsToSource, otherCalls) = partition ((== source) . callee) calls
      callers = map caller callsToSource
  in case callsToSource of
       [] -> []
       _  -> callers:(callers & map (dependents otherCalls) & concat)

dot :: [Call] -> [String] -> [String] -> String
dot calls sources sinks =
    let sinkFilteredCalls = if sinks /= [] then filterBySinks sinks calls else calls
        filteredCalls = if sources /= [] then filterBySources sources sinkFilteredCalls else sinkFilteredCalls
        moduleStr = formatModules filteredCalls
        callStr = formatCalls filteredCalls
    in intercalate "\n" [ "digraph {"
                        , "graph [ranksep = 4.0]"
                        , moduleStr
                        , callStr
                        , "}"
                        ]

filterBySinks :: [String] -> [Call] -> [Call]
filterBySinks [] _calls = []
filterBySinks sinks calls =
  let (sinkCalls, others) = partition (\call -> ((callee call) `elem` sinks)) calls
      newSinks = map caller sinkCalls
  in sinkCalls ++ filterBySinks newSinks others

filterBySources :: [String] -> [Call] -> [Call]
filterBySources [] _calls = []
filterBySources sources calls =
  let (sourceCalls, others) = partition (\call -> ((caller call) `elem` sources)) calls
      newSources = map callee sourceCalls
  in sourceCalls ++ filterBySources newSources others

formatCalls :: [Call] -> String
formatCalls calls =
  intercalate "\n" $ map formatCall calls
  where formatCall (source, target) = "\"" <> source <> "\" -> \"" <> target <> "\";"

formatModules :: [Call] -> String
formatModules calls =
  intercalate "\n" $ map formatMod $ Map.toList $ aggregateMods $ calls
  where formatMod (m, funs) = intercalate "\n" ["subgraph \"cluster_" <> m <> "\" {"
                                               , "label = \"" <> m <> "\";"
                                               , "style = \"rounded, filled\";"
                                               , "color = lightgrey;"
                                               , "node [style=filled, color=white];"
                                               , (intercalate "\n" $ map (formatFunction m) $ Set.toList funs)
                                               , "}"
                                               ]
        formatFunction m f =
          "\"" <> m <> "." <> f <> "\" [label = \"" <> f <> "\"]"
        aggregateMods =
          foldl updateModMap (Map.fromList []) . fmap (bimap (splitMod) (splitMod))
        updateModMap m ((sourcem, sourcef), (targetm, targetf)) =
          Map.insertWith (Set.union) targetm (Set.singleton targetf) $
          Map.insertWith (Set.union) sourcem (Set.singleton sourcef) $ m
        splitMod str =
          let lastDot = last $ findIndices (== '.') str
          in (take lastDot str, drop (lastDot + 1) str)

caller :: Call -> String
caller = fst

callee :: Call -> String
callee = snd
