module LocateCss where

import Test.QuickCheck
import CssTypes
import ParseCss
import DomNode

locateCss :: String -> DomNode -> [DomNode]
locateCss selector =
  let css = parseSelector selector
  in map last . filter (pathMatch css) . allPaths

allPaths :: DomNode -> [[DomNode]]
allPaths (Text _) = []
allPaths node = [[node]] ++ (map (node:) $ concatMap allPaths (nodeChildren node))

pathMatch :: CssSelector -> [DomNode] -> Bool
pathMatch css nodes@(n:ns) = case css of
  AnyMatch -> True
  NameMatch n -> n == nodeName (last nodes)
  ClassMatch c -> c `elem` nodeClasses (last nodes)
  IdMatch id -> Just id == nodeId (last nodes)
  CombinedMatch a b -> pathMatch a nodes && pathMatch b nodes
  DescendantMatch a b -> any (splitMatch a b) (splits nodes)
    where
      splits xs = [ splitAt n xs | n <- [0..length xs] ]
      splitMatch a b (as, bs) = pathMatch a as && pathMatch b bs
pathMatch _ [] = False
