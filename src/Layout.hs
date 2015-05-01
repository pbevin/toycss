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
layout = Set.fromList . getDimensions . recalcHeight . paint 1024 0 . toDom

paint :: Width -> YPos -> DomNode -> DomNode
paint w y node
  | isTextNode node =
      node { boundingBox = move 0 y $ rect (textWidth $ nodeText node) (calcFontSize node) }
  | otherwise =
      let y' = y + calcMarginTop node
          y'' = y' + calcPaddingTop node
          paintedNodes = paintChildren w y'' (children node)
          boxes = map boundingBox paintedNodes
      in (fixDisplayNone . recalcWidth w . recalcHeight) node {
           boundingBox = boundingBoxAll ((D y'' 0 y'' 0):boxes),
           children    = paintedNodes }

paintChildren :: Width -> YPos -> [DomNode] -> [DomNode]
paintChildren w y nodes = f w y nodes
  where f = if allInline nodes then paintInline else paintBlock

paintBlock :: Width -> YPos -> [DomNode] -> [DomNode]
paintBlock w y [] = []
paintBlock w y (node:nodes) =
  let node' = paint w y node
  in node' : paintBlock w (bottom (boundingBox node') + calcMarginBottom node) nodes

paintInline :: Width -> YPos -> [DomNode] -> [DomNode]
paintInline w y nodes = paintInline' w 0 y nodes
  where paintInline' w x y [] = []
        paintInline' w x y (node:nodes) =
          let node' = resizeNode (moveRight x) $ paint w y node
          in node' : paintInline' w (right $ boundingBox node') y nodes

allInline :: [DomNode] -> Bool
allInline = all (== Inline) . map (display . properties)

calcMarginTop :: DomNode -> Height
calcMarginTop node = calcHeight node (marginTop $ properties node)

calcMarginBottom :: DomNode -> Height
calcMarginBottom node = calcHeight node (marginBottom $ properties node)

calcPaddingTop :: DomNode -> Height
calcPaddingTop node = calcHeight node (paddingTop $ properties node)

calcPaddingBottom :: DomNode -> Height
calcPaddingBottom node = calcHeight node (paddingBottom $ properties node)

calcHeight :: DomNode -> Size -> Height
calcHeight node size = case size of
  Em a -> a * fs
  Pct a -> a * fs / 100
  Px p -> p
  SizeAuto -> 0
  where fs = fontSize (properties node)


recalcWidth :: Width -> DomNode -> DomNode
recalcWidth w node = node { boundingBox = setWidth (calcSize node w) (boundingBox node) }

recalcHeight :: DomNode -> DomNode
recalcHeight node = 
  let h = case (domHeight node) of
            Px h -> h
            _ -> boxHeight (boundingBox node)
      pb = calcPaddingBottom node
  in node { boundingBox = setHeight (h+pb) (boundingBox node) }
fixDisplayNone :: DomNode -> DomNode
fixDisplayNone node = case (display $ properties node) of
  None -> node { boundingBox = zero }
  _ -> node

calcSize :: DomNode -> Width -> Width
calcSize node w = case (domWidth node) of
  Pct p -> w * p / 100
  Px p -> p
  SizeAuto -> measureWidth node

calcFontSize :: DomNode -> Height
calcFontSize node = fontSize $ properties node

getDimensions :: DomNode -> [TaggedDimensions]
getDimensions node =
  if isTextNode node
  then []
  else domTag node : concatMap getDimensions (children node)

domTag :: DomNode -> TaggedDimensions
domTag node = tag (domId node) (boundingBox node)

resizeNode :: (Dimensions -> Dimensions) -> DomNode -> DomNode
resizeNode f node = node { boundingBox = f (boundingBox node) }
