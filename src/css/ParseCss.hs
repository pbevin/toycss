module Css.ParseCss where

import Css.CssTypes

parseSelector :: String -> CssSelector
parseSelector sel = map parseSingle $ words sel

parseSingle sel = map parseMatch $ cssWords sel

parseMatch ('.':cls) = Class cls
parseMatch ('#':id)  = Id id
parseMatch name      = Elem name


cssWords :: String -> [String]
cssWords str = filter (not . null) $ cssWords' "" str
  where
    cssWords' str [] = [str]
    cssWords' str ('.':xs) = [str] ++ cssWords' "." xs
    cssWords' str ('#':xs) = [str] ++ cssWords' "#" xs
    cssWords' str (x:xs) = cssWords' (str ++ [x]) xs
