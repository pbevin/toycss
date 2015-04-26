module GenHtml where

import Control.Monad.State
import Test.QuickCheck
import Text.PrettyPrint
import CssTypes
import DomNode
import GenCss
import GenDom
import ShowCss
import ShowDom

newtype HtmlDoc = HtmlDoc ([CssRule], [DomNode]) deriving Show
htmldoc :: [CssRule] -> [DomNode] -> HtmlDoc
htmldoc css dom = HtmlDoc (css, dom)

instance Arbitrary HtmlDoc where
  arbitrary = genHtml

genHtml :: Gen HtmlDoc
genHtml = do
  css <- arbitrary
  dom <- arbitrary
  return $ HtmlDoc (css, addIdsToDom dom)

renderHtml :: HtmlDoc -> String
renderHtml (HtmlDoc (css, dom)) = render $ pphtml css dom


h :: IO ()
h  = generate genHtml >>= putStrLn . renderHtml

content :: String -> Doc -> Doc
content name doc = vcat [ text ("<" ++ name ++ ">"),
                      nest 2 doc,
                      text ("</" ++ name ++ ">") ]

pphtml :: [CssRule] -> [DomNode] -> Doc
pphtml css dom = content "html" $ hhead css $$ hbody dom

hhead css = content "head" (reset $$ content "style" (vcat $ map (text . showcss) css))
hbody dom = content "body" (vcat $ map showdom dom)

reset = text "<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/meyer-reset/2.0/reset.min.css\">"

addIdsToDom :: [DomNode] -> [DomNode]
addIdsToDom nodes = fst $ runState (addIds nodes) 0

addIds :: [DomNode] -> State Int [DomNode]
addIds nodes = mapM addIds' nodes

addIds' :: DomNode -> State Int DomNode
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
