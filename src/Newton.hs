{-# LANGUAGE UnicodeSyntax #-}
module Newton (getNewtonColor, newtonPicSize) where

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

roots :: [Cex]
roots = [1:+0, mkPolar 1 (2*pi/3), mkPolar 1 (4*pi/3)]

--eps = 0.0001
nearTo :: Cex → Maybe Int
nearTo z | z==0 = Just 3 -- magnitude z < eps = Just 3
nearTo z = findIndex isNear roots where
  isNear r = magnitude (z - r) < 0.5

takeFirst :: [a] → (a → Maybe b) → (Int, b)
takeFirst l f = head $ mapMaybe (\(x,y) → (,) x <$> f y) (zip [0..] l)

-- z ↦ which root it converges to (0/1/2); 3 if diverges
cls :: Cex → (Int, Int)
cls z = takeFirst (iter z) nearTo

newtonPicSize = 2000 :: Int
range = 2

getNewtonColor :: (Int, Int) → (Int, Int)
getNewtonColor (a,b) = cls z where
  z = za :+ zb
  za = norm a
  zb = norm b
  norm :: Int → Double
  norm s = (2 * fromIntegral s / fromIntegral newtonPicSize - 1) * fromIntegral range
