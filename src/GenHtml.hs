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

type HtmlDoc = ([CssRule], [DomNode])

genHtml :: Gen HtmlDoc
genHtml = arbitrary

renderHtml :: HtmlDoc -> String
renderHtml (css, dom) = render $ pphtml css (addIdsToDom dom)


h :: IO ()
h  = generate genHtml >>= putStrLn . renderHtml

tag :: String -> Doc -> Doc
tag name doc = vcat [ text ("<" ++ name ++ ">"),
                      nest 2 doc,
                      text ("</" ++ name ++ ">") ]

pphtml :: [CssRule] -> [DomNode] -> Doc
pphtml css dom = tag "html" $ vcat [ hhead css, hbody dom ]

hhead css = tag "head" (tag "style" (vcat $ map (text . showcss) css))
hbody dom = tag "body" (vcat $ map showdom dom)

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
