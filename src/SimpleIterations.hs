{-# LANGUAGE UnicodeSyntax, MultiParamTypeClasses #-}
module SimpleIterations (snce, visu, bifu, Config(..)) where

import Graphics.Rendering.Chart.Easy

--class MyConf where
--  x :: Double
--  y :: Double

data Config = Config {
    r :: Double
  , x₀ :: Double
  }
--r, x₀ :: Double
--r  = 2.8
--x₀ = 0.5

φ c x = (r c) * x * (1 - x)
iter c = iterate (φ c) (x₀ c)
snce :: Config → EC (Layout Double Double) ()
snce c = do
  layout_y_axis . laxis_generate .= scaledAxis def (0,1)
  plot (line "" [take 50 $ zip [(0::Double)..] $ iter c])

range = [0,0.05..1]
plt :: (Double → Double) → [(Double, Double)]
plt f = map (\x → (x, f x)) range


-- x₀,0 → x₀,x₁ → x₁,x₁ → x₁,x₂ → x₂,x₂ → …
-- x,y → x,φx → φx,φx → …
visup c = next ((x₀ c), 0) where
  next (x, y) = (x, y) : (x, (φ c) x) : next (φ c x, φ c x)

visu :: Config → EC (Layout Double Double) ()
visu c = do
  layout_x_axis . laxis_generate .= scaledAxis def (0,1)
  layout_y_axis . laxis_generate .= scaledAxis def (0,1)
  plot (line "" [plt (φ c)])
  plot (line "" [plt id])
  plot (line "" [take 50 $ visup c])


φr :: Double → Double → Double
φr r x = r * x * (1 - x)

pres = 500
post = 200

atrs :: Double → [Double]
atrs r = take post $ drop pres $ iterate (φr r) 0.5

bifup :: [(Double, Double)]
bifup = concatMap (\x → map ((,) x) (atrs x)) [(1.0::Double),1.001..4]

points' :: [(x,y)]  -> EC l (PlotPoints x y)
points' values = liftEC $ do
    color <- takeColor
    shape <- takeShape
    plot_points_values .= values
    plot_points_title .= ""
    plot_points_style . point_color .= color
    plot_points_style . point_shape .= shape
    plot_points_style . point_radius .= 0.5

bifu :: EC (Layout Double Double) ()
bifu = do
  setColors [black `withOpacity` 0.4]
  setShapes [PointShapeCircle]
  plot (points' bifup)
