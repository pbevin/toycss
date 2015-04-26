{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Layout where

import Control.Applicative
import Control.Monad.Writer
import Data.Set (Set)
import qualified Data.Set as Set
import DomNode
import CssTypes
import GenHtml
import TimesRoman
import Debug.Trace


-- Top, Right, Bottom, Left
type TaggedDimensions = (String, Double, Double, Double, Double)
type Dimensions = (Double, Double, Double, Double)

data DisplayType = DisplayInline | DisplayBlock
data Size = SizePercent Double | SizeAuto

newtype Layout a = Layout {
  unLayout :: Writer (Set TaggedDimensions) a
} deriving (Monad, MonadWriter (Set TaggedDimensions), Functor, Applicative)

rect :: Double -> Double -> Dimensions
rect h v = (0, h, v, 0)

boundingBox :: [Dimensions] -> Dimensions
boundingBox [] = (0,0,0,0)
boundingBox [d] = d
boundingBox ((t,r,b,l):ds) = (min t t', max r r', max b b', min l l')
  where (t',r',b',l') = boundingBox ds

runLayout :: Layout a -> (a, Set TaggedDimensions)
runLayout = runWriter . unLayout

tag :: String -> Dimensions -> TaggedDimensions
tag name (t,r,b,l) = (name, t, r, b, l)

setHeight :: Double -> Dimensions -> Dimensions
setHeight h (t,r,_,l) = (t,r,t+h,l)

writeInline :: String -> Dimensions -> Dimensions
writeInline str _ = rect (textWidth str) 16
  -- "Vestibulum vel ante" -> rect 128.765625 16
  -- _ -> rect 166.91288 16

defaultDisplay :: String -> DisplayType
defaultDisplay "a" = DisplayInline
defaultDisplay _ = DisplayBlock

defaultHeight :: String -> Size
defaultHeight "body" = SizePercent 100
defaultHeight _ = SizeAuto

initialSize = (0, 1024, 768, 0)

bodyElement dom = Node (NodeAttrs "body" (Just "body") []) dom

layout :: HtmlDoc -> Set TaggedDimensions
layout doc = snd $ runLayout (f doc)
  where f (HtmlDoc (css, dom)) = paint initialSize css (bodyElement dom)

paint :: Dimensions -> [CssRule] -> DomNode -> Layout Dimensions
paint cdim _ (Text text) = return $ writeInline text cdim
paint cdim css dom = do
  ds <- mapM (paint cdim css) $ nodeChildren dom

  let ndim = resizeToBox dom (boundingBox ds) cdim
  logDim $ tag (maybe "" id $ nodeId dom) ndim
  return ndim


resizeToBox :: DomNode -> Dimensions -> Dimensions -> Dimensions
resizeToBox node (nt,nr,nb,nl) (ct,cr,cb,cl) = (t,r,b,l) where
  (l,r) = case defaultDisplay (nodeName node) of
    DisplayBlock -> (cl,cr)
    DisplayInline -> (nl,nr)
  (t,b) = case defaultHeight (nodeName node) of
    SizeAuto -> (nt,nb)
    _ -> (ct,cb)


logDim :: TaggedDimensions -> Layout ()
logDim d = tell (Set.singleton d)

