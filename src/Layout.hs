{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Layout where

import Control.Applicative
import Control.Monad.Writer
import Data.Set (Set)
import qualified Data.Set as Set
import Dimensions
import Html.HtmlNode
import Css.CssTypes
import HtmlDoc
import TimesRoman
import Debug.Trace


layout :: HtmlDoc -> Set TaggedDimensions
layout = Set.fromList . getDimensions . recalcHeight 768 . paint 1024 0 . toDom

paint :: Width -> YPos -> DomNode -> DomNode
paint w y node
  | isTextNode node =
      node { boundingBox = move 0 y $ rect (textWidth $ nodeText node) 16 }
  | otherwise =
      let paintedNodes = paintChildren w y (children node)
      in recalcWidth w node {
           boundingBox = boundingBoxAll (map boundingBox paintedNodes),
           children    = paintedNodes }

paintChildren :: Width -> YPos -> [DomNode] -> [DomNode]
paintChildren w y nodes = f w y nodes
  where f = if allInline nodes then paintInline else paintBlock

paintBlock :: Width -> YPos -> [DomNode] -> [DomNode]
paintBlock w y [] = []
paintBlock w y (node:nodes) =
  let node' = paint w y node
  in node' : paintBlock w (bottom $ boundingBox node') nodes


paintInline :: Width -> YPos -> [DomNode] -> [DomNode]
paintInline w y nodes = paintInline' w 0 y nodes
  where paintInline' w x y [] = []
        paintInline' w x y (node:nodes) =
          let node' = resizeNode (moveRight x) $ paint w y node
          in node' : paintInline' w (right $ boundingBox node') y nodes


allInline :: [DomNode] -> Bool
allInline = all (== Inline) . map (display . properties)

recalcWidth :: Width -> DomNode -> DomNode
recalcWidth w node = node { boundingBox = setWidth (calcSize node w) (boundingBox node) }

recalcHeight :: Height -> DomNode -> DomNode
recalcHeight h node  = node { boundingBox = setHeight h (boundingBox node) }

calcSize :: DomNode -> Width -> Width
calcSize node w = case (domWidth node) of
  Pct p ->  w * p / 100
  Px p -> p
  SizeAuto -> measureWidth node

getDimensions :: DomNode -> [TaggedDimensions]
getDimensions node =
  if isTextNode node
  then []
  else domTag node : concatMap getDimensions (children node)

domTag :: DomNode -> TaggedDimensions
domTag node = tag (domId node) (boundingBox node)

resizeNode :: (Dimensions -> Dimensions) -> DomNode -> DomNode
resizeNode f node = node { boundingBox = f (boundingBox node) }
