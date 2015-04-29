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


data DisplayType = DisplayInline | DisplayBlock
data StyleSize = SizePercent Double | SizeAuto

newtype Layout a = Layout {
  unLayout :: Writer (Set TaggedDimensions) a
} deriving (Monad, MonadWriter (Set TaggedDimensions), Functor, Applicative)

boundingBoxAll :: [Dimensions] -> Dimensions
boundingBoxAll [] = (0,0,0,0)
boundingBoxAll [d] = d
boundingBoxAll ((t,r,b,l):ds) = (min t t', max r r', max b b', min l l')
  where (t',r',b',l') = boundingBoxAll ds

runLayout :: Layout a -> (a, Set TaggedDimensions)
runLayout = runWriter . unLayout

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
  return (boundingBoxAll boxes)

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


logDim :: HtmlNode -> Dimensions -> Layout ()
logDim (Text _) _ = return ()
logDim node box = tell (Set.singleton tagged)
  where tagged = tag (maybe "" id $ nodeId node) box
