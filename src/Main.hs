{-# LANGUAGE UnicodeSyntax #-}
module Main where

import SimpleIterations
import Graphics.UI.Gtk

main = do
  initGUI
  window ← windowNew
  widgetShowAll window
  mainGUI


--main = do
--  --toFile def "1a-snce.png" snce
--  --toFile def "1b-visu.png" visu
--  toWindow 600 400 visu
--  toWindow 600 400 bifu
