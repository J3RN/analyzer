module Lib
    ( dfsPaths
    ) where

import Data.List (partition)

type Path = [String]

dfsPaths :: [(String, String)] -> String -> String -> [Path]
dfsPaths mappings source sink =
  let (children, others) = partition ((== source) . fst) mappings
      (finished, unfinished) = partition ((== sink) . snd) children
      furtherPaths = concat $ map (progressPaths others) unfinished
  in (map (\x -> [fst x, snd x]) finished) ++ furtherPaths
  where progressPaths ms (so, si) = map (so:) (dfsPaths ms si sink)
