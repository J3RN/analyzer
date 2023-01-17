module Lib
    ( dfsPaths
    , callers
    , callees
    , Mappings
    ) where

import Data.List (partition)
import Data.Function ((&))

type Path = [String]
type Mappings = [(String, String)]

dfsPaths :: Mappings -> String -> String -> [Path]
dfsPaths mappings source sink =
  let (children, others) = partition ((== source) . fst) mappings
      (finished, unfinished) = partition ((== sink) . snd) children
      furtherPaths = concat $ map (progressPaths others) unfinished
  in (map (\x -> [fst x, snd x]) finished) ++ furtherPaths
  where progressPaths ms (so, si) = map (so:) (dfsPaths ms si sink)

callers :: Mappings -> String -> [String]
callers mappings callee =
  mappings & filter((== callee) . snd) & map fst

callees :: Mappings -> String -> [String]
callees mappings caller =
  mappings & filter((== caller) . fst) & map snd
