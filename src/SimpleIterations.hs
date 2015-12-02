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
snce :: Config → EC (Layout Int Double) ()
snce c = plot (line "" [take 50 $ zip [(0::Int)..] $ iter c])

range = [0,0.05..1]
plt :: (Double → Double) → [(Double, Double)]
plt f = map (\x → (x, f x)) range


-- x₀,0 → x₀,x₁ → x₁,x₁ → x₁,x₂ → x₂,x₂ → …
-- x,y → x,φx → φx,φx → …
visup c = next ((x₀ c), 0) where
  next (x, y) = (x, y) : (x, (φ c) x) : next (φ c x, φ c x)

visu :: Config → EC (Layout Double Double) ()
visu c = do
  plot (line "" [plt (φ c)])
  plot (line "" [plt id])
  plot (line "" [take 50 $ visup c])



φr :: Double → Double → Double
φr r x = r * x * (1 - x)

pres = 500
post = 100

atrs :: Double → [Double]
atrs r = take post $ drop pres $ iterate (φr r) 0.5

bifup :: [(Double, Double)]
bifup = concatMap (\x → map ((,) x) (atrs x)) [(1.0::Double),1.003..4]

bifu :: EC (Layout Double Double) ()
bifu = do
  setColors [black `withOpacity` 0.4]
  setShapes [PointShapePlus]
  plot (points "" bifup)
