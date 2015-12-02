{-# LANGUAGE UnicodeSyntax #-}
module Main where

import SimpleIterations
import Control.Monad.IO.Class                (liftIO)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.Enums         (UpdateType(..))
import Graphics.Rendering.Chart.Easy         (execEC)
import Graphics.Rendering.Chart.Gtk          (updateCanvas)
import Graphics.Rendering.Chart.Renderable   (toRenderable)

--currentPlot :: Label → IO (Config → EC (Layout Double Double) ())
currentPlot config label = do
  l ← labelGetLabel label
  return $ case l of
    "Current: SNCE" → snce config
    "Current: VISU" → visu config
    _               → snce config

main :: IO ()
main = do
  initGUI

  window      ← windowNew
  vbox        ← vBoxNew False 5
  hboxpanel   ← hBoxNew False 5
  button1     ← buttonNewWithLabel "SNCE"
  button2     ← buttonNewWithLabel "VISU"
  button3     ← buttonNewWithLabel "BIFU"
  adj_r       ← adjustmentNew 0.0 0.0 4.0 0.01 1.0 0.0
  adj_x₀      ← adjustmentNew 0.0 0.0 1.0 0.01 1.0 0.0
  scroller_r  ← hScaleNew adj_r
  scroller_x₀ ← hScaleNew adj_x₀

  -- quit button doesn't work :(quitButton ← buttonNewWithLabel "Quit"
  panellabel ← labelNew $ Just "Current: SNCE"
  delim1 ← hSeparatorNew

  onDestroy window mainQuit
  window `on` focus $ \directiont →
    putStrLn ("Focused" ++ show directiont) >> return False
  set window [
    windowDefaultWidth := 800,
    windowDefaultHeight := 600,
    containerChild := vbox,
    containerBorderWidth := 10,
    windowTitle := "HW1"]

  boxPackStart vbox panellabel PackNatural 0
  miscSetAlignment panellabel 0 0
  boxPackStart vbox hboxpanel PackNatural 0

  boxPackStart hboxpanel button1 PackNatural 0
  boxPackStart hboxpanel button2 PackNatural 0
  boxPackStart hboxpanel button3 PackNatural 0

  set scroller_r  [scaleDigits := 2]
  set scroller_x₀ [scaleDigits := 2]
  boxPackStart hboxpanel scroller_r PackGrow 0
  boxPackStart hboxpanel scroller_x₀ PackGrow 0
  --scaleSetDigits scroller_r 0
  --scaleSetDigits scroller_x₀ 0

  boxPackStart vbox delim1 PackNatural 5

  area ← drawingAreaNew
  boxPackStart vbox area PackGrow 20

  let
    drawPic x = updateCanvas (toRenderable $ execEC $ x) area >> return ()
    refreshPic includeBifu = do
      l ← labelGetLabel panellabel
      if l == "Current: BIFU" && not includeBifu
      then return ()
      else do
        r  ← adjustmentGetValue adj_r
        x₀ ← adjustmentGetValue adj_x₀
        drawPic =<< currentPlot (Config r x₀) panellabel

  onValueChanged adj_r  $ refreshPic False
  onValueChanged adj_x₀ $ refreshPic False

  onClicked button1 $ labelSetLabel panellabel "Current: SNCE" >> refreshPic False
  onClicked button2 $ labelSetLabel panellabel "Current: VISU" >> refreshPic False
  onClicked button3 $ labelSetLabel panellabel "Current: BIFU" >> drawPic bifu

  window `on` configureEvent $ do
    (w, h) ← eventSize
    liftIO . putStrLn $ "Resizing: " ++ show w ++ " " ++ show h
    --liftIO $ refreshPic True
    return False

  --boxPackStart vbox delim2 PackNatural 5
  --boxPackStart vbox quitButton PackNatural 0

  widgetShowAll window
  mainGUI
