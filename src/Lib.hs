module Lib
    ( paths
    ) where

import Data.List (partition)

type Path = [String]

paths :: [(String, String)] -> String -> String -> [Path]
paths mappings source sink =
  let (children, others) = partition ((== source) . fst) mappings
      (finished, unfinished) = partition ((== sink) . snd) children
      furtherPaths = concat $ map (progressPaths others) unfinished
  in (map (\x -> [fst x, snd x]) finished) ++ furtherPaths
  where progressPaths ms (so, si) = map (so:) (paths ms si sink)
