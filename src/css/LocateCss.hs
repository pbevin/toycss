module Css.LocateCss where

import Test.QuickCheck
import Css.CssTypes
import Css.ParseCss
import Html.HtmlNode

locateCss :: String -> HtmlNode -> [HtmlNode]
locateCss selector =
  let css = parseSelector selector
  in map last . filter (pathMatch css) . allPaths

matchCss :: [CssRule] -> [HtmlNode] -> [CssRule]
matchCss css path = filter pm css
  where pm (sel, rules) = pathMatch sel path

allPaths :: HtmlNode -> [[HtmlNode]]
allPaths (Text _) = []
allPaths node = [[node]] ++ (map (node:) $ concatMap allPaths (nodeChildren node))

pathMatch :: CssSelector -> [HtmlNode] -> Bool
pathMatch _ [] = False
pathMatch [s] ns = nodeMatch s (last ns)
pathMatch (s:ss) (n:ns) =
  if nodeMatch s n
  then pathMatch ss ns
  else pathMatch (s:ss) ns

nodeMatch :: [CssNodeSpec] -> HtmlNode -> Bool
nodeMatch [] n = True
nodeMatch (s:ss) n = nodeMatch ss n && case s of
  Elem e  -> e == nodeName n
  Class c -> c `elem` nodeClasses n
  Id id   -> Just id == nodeId n
