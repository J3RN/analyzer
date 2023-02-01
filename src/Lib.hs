module Lib
    ( dfsPaths
    , callers
    , callees
    , dependencies
    , dependents
    , Calls
    ) where

import Data.List (partition)
import Data.Function ((&))

type Path = [String]
type Calls = [(String, String)]

dfsPaths :: Calls -> String -> String -> [Path]
dfsPaths calls source sink =
  let (children, others) = partition ((== source) . fst) calls
      (finished, unfinished) = partition ((== sink) . snd) children
      furtherPaths = concat $ map (progressPaths others) unfinished
  in (map (\x -> [fst x, snd x]) finished) ++ furtherPaths
  where progressPaths ms (so, si) = map (so:) (dfsPaths ms si sink)

callers :: Calls -> String -> [String]
callers calls callee =
  calls & filter((== callee) . snd) & map fst

callees :: Calls -> String -> [String]
callees calls caller =
  calls & filter((== caller) . fst) & map snd

dependencies :: Calls -> String -> [String]
dependencies calls source =
  let (childCalls, others) = partition ((== source) . fst) calls
  in (map snd childCalls) ++ (concat $ map (dependencies others) (map snd childCalls))

dependents :: Calls -> String -> [String]
dependents calls source =
  let (parentCalls, others) = partition ((== source) . snd) calls
  in (map fst parentCalls) ++ (concat $ map (dependents others) (map fst parentCalls))
