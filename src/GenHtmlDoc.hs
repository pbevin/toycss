module GenHtmlDoc where

import Control.Monad.State
import Test.QuickCheck
import Text.PrettyPrint
import Css.CssTypes
import Html.HtmlNode
import Css.GenCss
import Html.GenHtml
import Css.ShowCss
import Html.ShowHtml

newtype HtmlDoc = HtmlDoc ([CssRule], [HtmlNode]) deriving Show
htmldoc :: [CssRule] -> [HtmlNode] -> HtmlDoc
htmldoc css html = HtmlDoc (css, html)

instance Arbitrary HtmlDoc where
  arbitrary = genHtml

genHtml :: Gen HtmlDoc
genHtml = do
  css <- arbitrary
  html <- arbitrary
  return $ HtmlDoc (css, addIdsToHtml html)

renderHtml :: HtmlDoc -> String
renderHtml (HtmlDoc (css, html)) = render $ ppdoc css html


h :: IO ()
h  = generate genHtml >>= putStrLn . renderHtml

content :: String -> Doc -> Doc
content name doc = vcat [ text ("<" ++ name ++ ">"),
                      nest 2 doc,
                      text ("</" ++ name ++ ">") ]

ppdoc :: [CssRule] -> [HtmlNode] -> Doc
ppdoc css html = content "html" $ hhead css $$ hbody html

hhead css = content "head" (reset $$ content "style" (vcat $ map (text . showcss) css))
hbody html = content "body" (vcat $ map showhtml html)

reset = text "<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/meyer-reset/2.0/reset.min.css\">"

addIdsToHtml :: [HtmlNode] -> [HtmlNode]
addIdsToHtml nodes = fst $ runState (addIds nodes) 0

addIds :: [HtmlNode] -> State Int [HtmlNode]
addIds nodes = mapM addIds' nodes

addIds' :: HtmlNode -> State Int HtmlNode
addIds' n = case n of
  Text _ -> return n
  Node attrs children -> do
    id <- nextId
    newChildren <- addIds children
    return $ Node (setId ("n" ++ show id) attrs) newChildren

nextId :: State Int Int
nextId = do
  id <- get
  put (id + 1)
  return id
