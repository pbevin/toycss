module LocateCss (locateCss) where

import Test.QuickCheck
import Dom

locateCss :: String -> DomNode -> [DomNode]
locateCss selector = map last . filter match . allPaths
  where match = pathMatch (words selector)

allPaths :: DomNode -> [[DomNode]]
allPaths (Text _) = []
allPaths node = [[node]] ++ (map (node:) $ concatMap allPaths (nodeChildren node))

pathMatch :: [String] -> [DomNode] -> Bool
pathMatch [] [] = True
pathMatch (x:xs) (n:ns) =
  if cssMatch x n
  then pathMatch xs ns || pathMatch (x:xs) ns
  else pathMatch (x:xs) ns
pathMatch _ _ = False

cssMatch :: String -> DomNode -> Bool
cssMatch x node = x == nodeName node
