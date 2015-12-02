{-# LANGUAGE UnicodeSyntax #-}
module Newton where

import Data.Complex
import Data.Maybe
import Data.List

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

type Cex = Complex Double

φ, φ' :: Cex → Cex
φ  z = z**3 - 1
φ' z = 3 * z**2

next :: Cex → Cex
next z = z - φ z / φ' z

iter :: Cex → [Cex]
iter = iterate next

alp :: [Cex]
alp = [ a :+ b | a ← l, b ← l ]
    where l = [start,start+step..end]
          start = -1
          end   = 1
          step  = 0.0625

eps = 0.1 :: Double

roots :: [Cex]
roots = [1:+0, mkPolar 1 (2*pi/3), mkPolar 1 (4*pi/3)]

-- z ↦ which root it converges to (0/1/2); 3 if diverges
cls :: Cex → Int
cls z = fromMaybe 3 $ findIndex isn roots where
  isn r = magnitude (z - r) < eps 

toPair :: Cex → (Double, Double)
toPair (a :+ b) = (a, b)
    
-- newt = _

-- main = toFile def "2-newt.png" newt
