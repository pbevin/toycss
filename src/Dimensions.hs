module Dimensions where

import Control.Applicative
import Data.Monoid

type XPos = Double
type YPos = Double
type Height = Double
type Width = Double

-- Top, Right, Bottom, Left
type TaggedDimensions = (String, Double, Double, Double, Double)
data Dimensions = D { top :: Double, right :: Double, bottom :: Double, left :: Double } deriving (Show, Eq)

instance Monoid Dimensions where
  mempty = zero
  mappend = dimUnion

rect :: Double -> Double -> Dimensions
rect h v = D 0 h v 0

sq :: Double -> Dimensions
sq s = rect s s

tag :: String -> Dimensions -> TaggedDimensions
tag name (D t r b l) = (name, t, r, b, l)

move :: XPos -> YPos -> Dimensions -> Dimensions
move x y (D t r b l) = D (t+y) (r+x) (b+y) (l+x)

moveDown :: YPos -> Dimensions -> Dimensions
moveDown = move 0

setWidth :: Width -> Dimensions -> Dimensions
setWidth w d = d { right = left d + w }

setHeight :: Height -> Dimensions -> Dimensions
setHeight h d = d { bottom = top d + h }

zero :: Dimensions
zero = D 0 0 0 0

boundingBoxAll :: [Dimensions] -> Dimensions
boundingBoxAll [] = zero
boundingBoxAll xs = foldl1 dimUnion xs

dimUnion :: Dimensions -> Dimensions -> Dimensions
dimUnion (D t r b l) (D t' r' b' l') = D (min t t') (max r r') (max b b') (min l l')
