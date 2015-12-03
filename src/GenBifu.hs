{-# LANGUAGE UnicodeSyntax #-}
module GenBifu where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

import SimpleIterations (bifu)

main = toFile def "3c-bifu.png" bifu
