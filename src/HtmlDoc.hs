module HtmlDoc where

import Css.CssTypes
import Html.HtmlNode
import Dimensions

data DomNode = DomNode { htmlAttrs :: NodeAttrs,
                         nodeText :: String,
                         properties :: BoxProperties,
                         boundingBox :: Dimensions,
                         children :: [DomNode] }
             deriving (Show, Eq)

newtype HtmlDoc = HtmlDoc ([CssRule], [HtmlNode]) deriving Show

htmlDoc :: [CssRule] -> [HtmlNode] -> HtmlDoc
htmlDoc css html = HtmlDoc (css, html)


toDom :: HtmlDoc -> DomNode
toDom (HtmlDoc (css,html)) = bodyNode zero $ map toDomNode html

bodyNode :: Dimensions -> [DomNode] -> DomNode
bodyNode = DomNode (attrs "body") "" bodyProperties

bodyProperties :: BoxProperties
bodyProperties = BoxProperties Block (Px 1024) (Px 768)

textProperties :: BoxProperties
textProperties = BoxProperties Inline SizeAuto SizeAuto

elemProperties :: NodeAttrs -> BoxProperties
elemProperties attrs =
  if nName attrs `elem` inlineElements
  then BoxProperties Inline SizeAuto SizeAuto
  else BoxProperties Block (Pct 100) SizeAuto

inlineElements = [ "a", "span" ]

textAttrs = NodeAttrs "_text" Nothing []

toDomNode :: HtmlNode -> DomNode
toDomNode (Text t) = DomNode textAttrs t textProperties zero []
toDomNode (Node attrs children) = DomNode attrs "" (elemProperties attrs) zero (map toDomNode children)

domId :: DomNode -> String
domId node = let attrs = htmlAttrs node in maybe (nName attrs) id (nId attrs)

isTextNode :: DomNode -> Bool
isTextNode node = domId node == "_text"

domWidth :: DomNode -> Size
domWidth = width . properties

measureWidth :: DomNode -> Width
measureWidth node = right d - left d where d = boundingBox node
