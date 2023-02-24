module Lib
    ( dfsPaths
    , dependencies
    , dependents
    , dot
    , Call
    ) where

import Data.Bifunctor (bimap)
import Data.List (findIndices, intercalate, partition)
import Data.Map()
import qualified Data.Map as Map
import Data.Set()
import qualified Data.Set as Set

type Path = [String]
type Call = (String, String)

dfsPaths :: [Call] -> String -> String -> [Path]
dfsPaths calls source sink =
  let (children, others) = partition ((== source) . caller) calls
      (finished, unfinished) = partition ((== sink) . callee) children
      furtherPaths = concat $ map (progressPaths others) unfinished
  in (map (\x -> [caller x, callee x]) finished) ++ furtherPaths
  where progressPaths ms (so, si) = map (so:) (dfsPaths ms si sink)

dependencies :: [Call] -> String -> [[String]]
dependencies calls source =
  let (childCalls, others) = partition ((== source) . caller) calls
  in case childCalls of
    [] -> []
    _ -> (map callee childCalls):(concat $ map (dependencies others) (map callee childCalls))

dependents :: [Call] -> String -> [[String]]
dependents calls source =
  let (parentCalls, others) = partition ((== source) . callee) calls
  in case parentCalls of
    [] -> []
    _ -> (map caller parentCalls):(concat $ map (dependents others) (map caller parentCalls))

dot :: [Call] -> [String] -> [String] -> String
dot calls _sources _sinks =
    let moduleStr = formatModules calls
        callStr = formatCalls calls
    in intercalate "\n" [ "digraph {"
                        , "graph [ranksep = 4.0]"
                        , moduleStr
                        , callStr
                        , "}"
                        ]

formatCalls :: [Call] -> String
formatCalls calls =
  let callLines = map (\(source, target) -> "\"" <> source <> "\" -> \"" <> target <> "\";") calls
  in intercalate "\n" callLines

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
