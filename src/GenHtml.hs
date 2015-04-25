module GenHtml where

import Test.QuickCheck
import Text.PrettyPrint
import CssTypes
import DomNode
import GenCss
import GenDom
import ShowCss
import ShowDom

genHtml :: Gen String
genHtml = do
  (css, dom) <- arbitrary
  return $ render $ pphtml css dom


h :: IO ()
h  = generate genHtml >>= putStrLn

tag :: String -> Doc -> Doc
tag name doc = vcat [ text ("<" ++ name ++ ">"),
                      nest 2 doc,
                      text ("</" ++ name ++ ">") ]

pphtml :: [CssRule] -> [DomNode] -> Doc
pphtml css dom = tag "html" $ vcat [ hhead css, hbody dom ]

hhead css = tag "head" (tag "style" (vcat $ map (text . showcss) css))
hbody dom = tag "body" (vcat $ map showdom dom)
