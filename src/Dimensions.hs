module Dimensions where

type YPos = Double
type Height = Double
type Width = Double

-- Top, Right, Bottom, Left
type TaggedDimensions = (String, Double, Double, Double, Double)
type Dimensions = (Double, Double, Double, Double)

rect :: Double -> Double -> Dimensions
rect h v = (0, h, v, 0)

sq :: Double -> Dimensions
sq s = rect s s

tag :: String -> Dimensions -> TaggedDimensions
tag name (t,r,b,l) = (name, t, r, b, l)

setWidth :: Width -> Dimensions -> Dimensions
setWidth w (t,r,b,l) = (t,l+w,b,l)

setHeight :: Height -> Dimensions -> Dimensions
setHeight h (t,r,b,l) = (t,r,t+h,l)

zero :: Dimensions
zero = (0,0,0,0)
