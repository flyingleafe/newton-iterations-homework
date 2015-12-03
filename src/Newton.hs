{-# LANGUAGE UnicodeSyntax #-}
module Newton (getNewtonColor) where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

import Data.Complex
import Data.Maybe
import Data.List

type Cex = Complex Double

φ, φ' :: Cex → Cex
φ  z = z**3 - 1
φ' z = 3 * z**2

next z = z - φ z / φ' z
iter = iterate next

alp :: [Cex]
alp = [ a :+ b | a ← l, b ← l ]
    where l = [start,start+step..end]
          start = -2
          end   = 2
          step  = 0.025

eps = 0.1 :: Double

roots :: [Cex]
roots = [1:+0, mkPolar 1 (2*pi/3), mkPolar 1 (4*pi/3)]

-- z ↦ which root it converges to (0/1/2); 3 if diverges
cls :: Cex → Int
cls z = fromMaybe 3 $ findIndex isn roots where
  isn r = magnitude ((iter z !! 30) - r) < eps

toPair :: Cex → (Double, Double)
toPair (a :+ b) = (a, b)

size = 600 :: Int

coords :: Int → Int → Cex
coords n m = norm n :+ norm m
       where
         norm :: Int → Double
         norm x = ((fromIntegral x / fromIntegral size) - 0.5) * 2

colors :: Int → Int → Int
colors n m = cls (coords n m)

newt :: EC (Layout Double Double) ()
newt = do
  setColors $ map opaque [red, green, blue, black]
  setShapes [PointShapePolygon 4 False]
  flip mapM_ [0,1,2,3]
    $ \i → plot (points "" $ map toPair $ filter ((==i) . cls) alp)

newr =
  flip mapM_ alp
    $ \z₀ → plot (line "" [map toPair $ take 20 $ iter z₀])

getNewtonColor :: Int → (Int, Int) → Int
getNewtonColor = undefined

main = do
  toFile def "2-newt.png" newt
  --toFile def "2-newr.png" newr
