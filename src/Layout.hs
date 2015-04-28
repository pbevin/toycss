{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Layout where

import Control.Applicative
import Control.Monad.Writer
import Data.Set (Set)
import qualified Data.Set as Set
import Html.HtmlNode
import Css.CssTypes
import GenHtmlDoc
import TimesRoman
import Debug.Trace


type Height = Double
type Width = Double

-- Top, Right, Bottom, Left
type TaggedDimensions = (String, Double, Double, Double, Double)
type Dimensions = (Double, Double, Double, Double)

data DisplayType = DisplayInline | DisplayBlock
data StyleSize = SizePercent Double | SizeAuto

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

setTop h (_,r,b,l) = (h,r,b,l)
setBottom h (t,r,_,l) = (t,r,h,l)

height :: Dimensions -> Double
height (t,_,b,_) = b-t
top (t,_,_,_) = t

lower :: Double -> Dimensions -> Dimensions
lower h (t,r,b,l) = (t+h,r,b+h,l)

writeInline :: String -> Dimensions -> Dimensions
writeInline str _ = rect (textWidth str) 16

defaultDisplay :: String -> DisplayType
defaultDisplay "a" = DisplayInline
defaultDisplay "span" = DisplayInline
defaultDisplay _ = DisplayBlock

defaultHeight :: String -> StyleSize
defaultHeight "body" = SizePercent 100
defaultHeight _ = SizeAuto

initialWidth = 1024
initialHeight = 768
initialSize = (0, initialWidth, initialHeight, 0)

bodyElement dom = Node (NodeAttrs "body" (Just "body") []) dom

layout :: HtmlDoc -> Set TaggedDimensions
layout doc = snd $ runLayout (f doc)
  where f (HtmlDoc (css, dom)) = paintRoot (bodyElement dom)


paintRoot :: HtmlNode -> Layout ()
paintRoot root = do
  paintMany initialWidth (nodeChildren root)
  logDim root initialSize
  return ()


paintMany :: Double -> [HtmlNode] -> Layout Dimensions
paintMany width nodes = do
  boxes <- mapM (paintOne width) nodes
  let boxes' = reverse $ snd $ foldl vertical (0,[]) boxes
  mapM (uncurry logDim) $ zip nodes boxes'
  return (boundingBox boxes)

vertical :: (Double,[Dimensions]) -> Dimensions -> (Double,[Dimensions])
vertical (h,ds) d@(t,r,b,l) = (h+height d,(t+h,r,b+h,l):ds)

nodeBox :: HtmlNode -> Double -> Dimensions -> Dimensions
nodeBox node width (t,r,b,l) = (t',r',b',l') where
  (l',r') = case defaultDisplay (nodeName node) of
    DisplayBlock -> (0, width)
    DisplayInline -> (l,r)
  (t',b') = (t,b)



paintOne :: Double -> HtmlNode -> Layout Dimensions
paintOne width (Text t) = return $ rect (textWidth t) 16
paintOne width node = do
  d <- paintMany width (nodeChildren node)
  return $ nodeBox node width d

-- paintBlock :: Dimensions -> [HtmlNode] -> Layout Dimensions
-- paintBlock cdim nodes = do
--   h <- foldM (paintChild cdim) (top cdim) nodes
--   let ndim = setBottom h cdim
--   return ndim


-- paintChild :: Dimensions -> Height -> HtmlNode -> Layout Height
-- paintChild cdim h (Text t) = return $ h+16
-- paintChild cdim h node = do
--   ndim <- paintBlock (setTop h cdim) (nodeChildren node)
--   logDim node ndim --$
--   return $ h + height ndim





-- paint :: Dimensions -> HtmlNode -> Layout Dimensions
-- paint cdim (Text text) = return $ writeInline text cdim
-- paint cdim dom = do
--   (h, ds) <- foldM (paintChild $ setHeight 0 cdim) (0, []) $ nodeChildren dom

--   let ndim = resizeToBox dom (boundingBox ds) cdim
--   logDim $ tag (maybe "" id $ nodeId dom) ndim
--   return ndim

-- paintChild :: Dimensions -> (Double, [Dimensions]) -> HtmlNode -> Layout (Double, [Dimensions])
-- paintChild cdim (h, dims) node = do
--   ndim <- paint (setTop h cdim) node
--   return $ (h + height ndim, ndim : dims)


resizeToBox :: HtmlNode -> Dimensions -> Dimensions -> Dimensions
resizeToBox node (nt,nr,nb,nl) (ct,cr,cb,cl) = (t,r,b,l) where
  (l,r) = case defaultDisplay (nodeName node) of
    DisplayBlock -> (cl,cr)
    DisplayInline -> (nl,nr)
  (t,b) = case defaultHeight (nodeName node) of
    SizeAuto -> (nt,nb)
    _ -> (ct,cb)


logDim :: HtmlNode -> Dimensions -> Layout ()
logDim (Text _) _ = return ()
logDim node box = tell (Set.singleton tagged)
  where tagged = tag (maybe "" id $ nodeId node) box
