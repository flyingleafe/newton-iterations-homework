{-# LANGUAGE UnicodeSyntax #-}
module Newton where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

import Data.Complex
import Data.Maybe
import Data.List

type Cex = Complex Double

φ, φ' :: Cex → Cex
φ  z = z**3 - 1
φ' z = 3 * z**2

next :: Cex → Cex
next z = z - φ z / φ' z

iter :: Cex → [Cex]
iter = iterate next

start = -2
end = 2
step = 0.005

alp :: [Cex]
alp = [ a :+ b | a ← l, b ← l ]
    where l = [start,start+step..end]

roots :: [Cex]
roots = [1:+0, mkPolar 1 (2*pi/3), mkPolar 1 (4*pi/3)]
      
eps = 0.0001
nearTo :: Cex → Maybe Int
nearTo z | z==0 = Just 3 -- magnitude z < eps = Just 3
nearTo z = findIndex isNear roots where
  isNear r = magnitude (z - r) < 0.5

takeFirst :: [a] → (a → Maybe b) → b
takeFirst l f = head $ mapMaybe f l

-- z ↦ which root it converges to (0/1/2); 3 if diverges
cls :: Cex → Int
cls z = takeFirst (iter z) nearTo

toPair :: Cex → (Double, Double)
toPair (a :+ b) = (a, b)
    
points' :: [(x,y)]  -> EC l (PlotPoints x y)
points' values = liftEC $ do
    color <- takeColor
    shape <- takeShape
    plot_points_values .= values
    plot_points_title .= ""
    plot_points_style . point_color .= color
    plot_points_style . point_shape .= shape
    plot_points_style . point_radius .= 1

newt = do
  layout_x_axis . laxis_generate .= scaledAxis def (start,end)
  layout_y_axis . laxis_generate .= scaledAxis def (start,end)
  setColors $ map opaque [red, green, blue, black]
  setShapes [PointShapePolygon 4 False]
  flip mapM_ [0,1,2,3]
    $ \i → plot (points' $ map toPair $ filter ((==i) . cls) alp)

main = do
  let a = toRenderable $ execEC newt
  renderableToFile (FileOptions (800, 800) PNG) "2-newt.png" a
