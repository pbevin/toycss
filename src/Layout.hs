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
layout doc = Set.fromList $ getDimensions $ recalcHeight 768 $ paint 1024 0 (toDom doc)

paint :: Width -> YPos -> DomNode -> DomNode
paint w y node =
  if isTextNode node
  then
    node { boundingBox = move 0 y $ rect (textWidth $ nodeText node) 16 }
  else
    let paintedNodes = paintBlock w y (children node)
    in recalcWidth w node { boundingBox = boundingBoxAll (map boundingBox paintedNodes),
                            children = paintedNodes }

paintBlock :: Width -> YPos -> [DomNode] -> [DomNode]
paintBlock w y [] = []
paintBlock w y (node:nodes) =
  let node' = paint w y node
  in node' : paintBlock w (bottom $ boundingBox node') nodes



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
