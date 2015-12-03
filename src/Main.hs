{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE BangPatterns  #-}
module Main where

import SimpleIterations
import Newton

import System.IO
import Data.Text                              (pack)
import Data.Word
import Data.Array.MArray
import Control.Monad.IO.Class                 (liftIO)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Pixbuf
import Graphics.Rendering.Chart.Easy          (execEC)
import Graphics.Rendering.Chart.Gtk           (updateCanvas)
import Graphics.Rendering.Chart.Renderable    (toRenderable)
import Graphics.Rendering.Chart.Backend.Cairo

bifuFileName = "Bifurcation.png"
newtFileName = "Newton.png"

--currentPlot :: Label → IO (Config → EC (Layout Double Double) ())
currentPlot config label = do
  l ← labelGetLabel label
  return $ case l of
    "Current: SNCE" → snce config
    "Current: VISU" → visu config
    _               → snce config


drawAndSaveBitmap :: IO ()
drawAndSaveBitmap = do
  let size = 1000
  pb ← pixbufNew ColorspaceRgb False 8 size size
  rowstride ← pixbufGetRowstride pb
  nChannels ← pixbufGetNChannels pb
  let address i j = j * rowstride + j * nChannels
  arr ← pixbufGetPixels pb :: IO (PixbufData Int Word8)
  mapM_ (\i → do
              writeArray arr (i*3) 255
              writeArray arr (i*3+1) 255
              writeArray arr (i*3+2) 255
        ) [0..(size-1)*(size-1)]
  pixbufSave pb ("test2.png" :: String) (pack "png") ([] :: [(String, String)])

main :: IO ()
main = do
  initGUI

  window      ← windowNew
  vbox        ← vBoxNew False 5
  hboxpanel   ← hBoxNew False 5
  button1     ← buttonNewWithLabel "SNCE"
  button2     ← buttonNewWithLabel "VISU"
  button3     ← buttonNewWithLabel "Generate BIFU"
  button4     ← buttonNewWithLabel "Generate NEWT"
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
  boxPackStart hboxpanel button4 PackNatural 0

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
    getPic = toRenderable . execEC
    drawPic x = updateCanvas x area >> return ()
    refreshPic quick = do
      l ← labelGetLabel panellabel
      if l == "Current: BIFU" && quick
      then return ()
      else do
        r  ← adjustmentGetValue adj_r
        x₀ ← adjustmentGetValue adj_x₀
        drawPic . getPic =<< currentPlot (Config r x₀) panellabel
    dumpToFile !name !dtype !label = do
      labelSetLabel panellabel label
      putStrLn $ "Calculating..., state " ++ label
      let !renderable = getPic $! dtype
      putStrLn "To file..."
      renderableToFile (FileOptions (1200, 600) PNG) name renderable
      putStrLn "Drawing on screen..."
      --drawPic renderable
      putStrLn "Done"
      return $! ()


  onValueChanged adj_r  $ refreshPic False
  onValueChanged adj_x₀ $ refreshPic False

  onClicked button1 $ labelSetLabel panellabel "Current: SNCE" >> refreshPic True
  onClicked button2 $ labelSetLabel panellabel "Current: VISU" >> refreshPic True
  --onClicked button3 $ dumpToFile bifuFileName bifu "Current: BIFU"
  onClicked button3 drawAndSaveBitmap
  onClicked button4 $ dumpToFile newtFileName newt "Current: NEWT"

  window `on` configureEvent $ do
    (w, h) ← eventSize
    liftIO . putStrLn $ "Resizing: " ++ show w ++ " " ++ show h
    liftIO $ refreshPic True
    return False

  widgetShowAll window
  mainGUI
