module ParseCss where

import CssTypes

parseSelector :: String -> CssSelector
parseSelector sel = foldl1 DescendantMatch (map parseSingle $ words sel)

parseSingle sel = foldl1 CombinedMatch (map parseMatch $ cssWords sel)

parseMatch ('.':cls) = ClassMatch cls
parseMatch ('#':id)  = IdMatch id
parseMatch name      = NameMatch name


cssWords :: String -> [String]
cssWords str = filter (not . null) $ cssWords' "" str
  where
    cssWords' str [] = [str]
    cssWords' str ('.':xs) = [str] ++ cssWords' "." xs
    cssWords' str ('#':xs) = [str] ++ cssWords' "#" xs
    cssWords' str (x:xs) = cssWords' (str ++ [x]) xs
