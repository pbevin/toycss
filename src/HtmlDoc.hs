module HtmlDoc where

import Css.CssTypes
import Css.LocateCss
import Html.HtmlNode
import Dimensions
import Debug.Trace

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
toDom (HtmlDoc (css,html)) = bodyNode zero $ map (toDomNode initialProperties css []) html

bodyNode :: Dimensions -> [DomNode] -> DomNode
bodyNode = DomNode (attrs "body") "" bodyProperties

bodyProperties :: BoxProperties
bodyProperties = BoxProperties Block (Px 1024) (Px 768) 16

textProperties :: BoxProperties
textProperties = BoxProperties Inline SizeAuto SizeAuto 16

elemProperties :: NodeAttrs -> BoxProperties
elemProperties attrs =
  if nName attrs `elem` inlineElements
  then BoxProperties Inline SizeAuto SizeAuto 16
  else BoxProperties Block (Pct 100) SizeAuto 16

inlineElements = [ "a", "span" ]

textAttrs = NodeAttrs "_text" Nothing []

toDomNode :: BoxProperties -> [CssRule] -> [HtmlNode] -> HtmlNode -> DomNode
toDomNode props css path node = let path' = path ++ [node] in case node of
  Text t ->
    DomNode textAttrs t ((props `merge` textProperties) `cssMerge` matchCss css path') zero []
  Node attrs children ->
    DomNode attrs "" myProps zero $ map recurse children
      where recurse = toDomNode myProps css path'
            myProps = (props `merge` elemProperties attrs) `cssMerge` matchCss css path'

domId :: DomNode -> String
domId node = let attrs = htmlAttrs node in maybe (nName attrs) id (nId attrs)

isTextNode :: DomNode -> Bool
isTextNode node = domId node == "_text"

domWidth :: DomNode -> Size
domWidth = width . properties

domHeight :: DomNode -> Size
domHeight = height . properties

measureWidth :: DomNode -> Width
measureWidth node = right d - left d where d = boundingBox node

initialProperties = BoxProperties { display = Block, width = SizeAuto, height = SizeAuto, fontSize = 16 }


merge :: BoxProperties -> BoxProperties -> BoxProperties
merge a b = b { fontSize = fontSize a }

cssMerge :: BoxProperties -> [CssRule] -> BoxProperties
cssMerge props css = foldl cssMerge' props css

cssMerge' :: BoxProperties -> CssRule -> BoxProperties
cssMerge' props (_, decls) = foldl cssMerge'' props decls

cssMerge'' :: BoxProperties -> CssDecl -> BoxProperties
cssMerge'' props decl = case decl of
  FontSize sz -> updateFontSize sz props
  Height sz -> updateHeight sz props
  _ -> props


updateFontSize :: Size -> BoxProperties -> BoxProperties
updateFontSize sz props = props { fontSize = scale sz (fontSize props) }

updateHeight sz props = props { height = sz }

scale :: Size -> Double -> Double
scale (Px p) _ = p
scale (Pct p) x = x * p / 100
scale (SizeAuto) x = x
