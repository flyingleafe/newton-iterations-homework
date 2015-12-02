{-# LANGUAGE UnicodeSyntax #-}
module SimpleIterations (snce, visu, bifu) where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Gtk

r, x₀ :: Double
r  = 2.8
x₀ = 0.5

φ :: Double → Double
φ x = r * x * (1 - x)

iter = iterate φ x₀

snce = plot (line "" [take 50 $ zip [(0::Int)..] iter])


range = [0,0.05..1]
plt :: (Double → Double) → [(Double, Double)]
plt f = map (\x → (x, f x)) range

-- x₀,0 → x₀,x₁ → x₁,x₁ → x₁,x₂ → x₂,x₂ → …
-- x,y → x,φx → φx,φx → …
visup = next (x₀,0) where
  next (x, y) = (x, y) : (x, φ x) : next (φ x, φ x)

visu = do
  plot (line "" [plt φ])
  plot (line "" [plt id])
  plot (line "" [take 50 visup])


φr :: Double → Double → Double
φr r x = r * x * (1 - x)

pres = 500
post = 100

atrs :: Double → [Double]
atrs r = take post $ drop pres $ iterate (φr r) 0.5

bifup :: [(Double, Double)]
bifup = concatMap (\x → map ((,) x) (atrs x)) [(1.0::Double),1.003..4]

bifu = do
  setColors [black `withOpacity` 0.4]
  setShapes [PointShapePlus]
  plot (points "" bifup)
