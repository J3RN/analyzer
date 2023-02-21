module Lib
    ( dfsPaths
    , dependencies
    , dependents
    , Calls
    ) where

import Data.List (partition)

type Path = [String]
type Calls = [(String, String)]

dfsPaths :: Calls -> String -> String -> [Path]
dfsPaths calls source sink =
  let (children, others) = partition ((== source) . fst) calls
      (finished, unfinished) = partition ((== sink) . snd) children
      furtherPaths = concat $ map (progressPaths others) unfinished
  in (map (\x -> [fst x, snd x]) finished) ++ furtherPaths
  where progressPaths ms (so, si) = map (so:) (dfsPaths ms si sink)

dependencies :: Calls -> String -> [[String]]
dependencies calls source =
  let (childCalls, others) = partition ((== source) . fst) calls
  in case childCalls of
    [] -> []
    _ -> (map snd childCalls):(concat $ map (dependencies others) (map snd childCalls))

dependents :: Calls -> String -> [[String]]
dependents calls source =
  let (parentCalls, others) = partition ((== source) . snd) calls
  in case parentCalls of
    [] -> []
    _ -> (map fst parentCalls):(concat $ map (dependents others) (map fst parentCalls))
