module Dimensions where

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
