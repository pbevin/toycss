module HtmlDoc where

import Css.CssTypes
import Html.HtmlNode
import Dimensions

data DomNode = DomNode { htmlAttrs :: NodeAttrs,
                         boundingBox :: Dimensions,
                         children :: [DomNode] }
             | DomText String
             deriving (Show, Eq)

newtype HtmlDoc = HtmlDoc ([CssRule], [HtmlNode]) deriving Show

htmlDoc :: [CssRule] -> [HtmlNode] -> HtmlDoc
htmlDoc css html = HtmlDoc (css, html)


toDom :: HtmlDoc -> DomNode
toDom (HtmlDoc (css,html)) = bodyNode zero $ map toDomNode html

zero :: Dimensions
zero = (0,0,0,0)

bodyNode :: Dimensions -> [DomNode] -> DomNode
bodyNode = DomNode (attrs "body")

toDomNode :: HtmlNode -> DomNode
toDomNode (Text t) = DomText t
toDomNode (Node attrs children) = DomNode attrs zero (map toDomNode children)
