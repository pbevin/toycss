module Dimensions where

import Data.Semigroup
import Data.List.NonEmpty

type XPos = Double
type YPos = Double
type Height = Double
type Width = Double

-- Top, Right, Bottom, Left
type TaggedDimensions = (String, Double, Double, Double, Double)
data Dimensions = D { top :: Double, right :: Double, bottom :: Double, left :: Double } deriving (Show, Eq)

instance Semigroup Dimensions where
  (<>) = dimUnion

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

moveRight :: YPos -> Dimensions -> Dimensions
moveRight x = move x 0

setWidth :: Width -> Dimensions -> Dimensions
setWidth w d = d { right = left d + w }

setHeight :: Height -> Dimensions -> Dimensions
setHeight h d = d { bottom = top d + h }

zero :: Dimensions
zero = D 0 0 0 0

boundingBoxAll :: [Dimensions] -> Dimensions
boundingBoxAll xs = sconcat $ fromList xs

dimUnion :: Dimensions -> Dimensions -> Dimensions
dimUnion (D t r b l) (D t' r' b' l') = D (min t t') (max r r') (max b b') (min l l')
