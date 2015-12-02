{-# LANGUAGE UnicodeSyntax #-}
module Main where

import SimpleIterations
import Graphics.UI.Gtk
import Control.Monad.IO.Class(liftIO)
import Graphics.Rendering.Chart.Easy (execEC)
import Graphics.Rendering.Chart.Gtk (updateCanvas)
import Graphics.Rendering.Chart.Renderable (toRenderable)

main :: IO ()
main = do
  initGUI

  window ← windowNew
  hboxpanel ← hBoxNew False 5
  vbox ← vBoxNew False 10
  button1 ← buttonNewWithLabel "SNCE"
  button2 ← buttonNewWithLabel "VISU"
  button3 ← buttonNewWithLabel "BIFU"

  -- quit button doesn't work :(
  quitButton ← buttonNewWithLabel "Quit"
  panellabel ← labelNew $ Just "Control panel:"
  delim1 ← hSeparatorNew
  delim2 ← hSeparatorNew

  onDestroy window mainQuit
  window `on` focus $ \directiont →
    putStrLn ("Focused" ++ show directiont) >> return False
  window `on` configureEvent $ do
    (w, h) ← eventSize
    liftIO . putStrLn $ "Resize: " ++ show w ++ " " ++ show h
    return False
  set window [
    windowDefaultWidth := 800,
    windowDefaultHeight := 600,
    containerChild := vbox,
    containerBorderWidth := 10,
    windowTitle := "HW1"]

  boxPackStart vbox panellabel PackNatural 0
  miscSetAlignment panellabel 0 0
  boxPackStart vbox hboxpanel PackNatural 0

  boxPackStart hboxpanel button1 PackRepel 0
  boxPackStart hboxpanel button2 PackRepel 0
  boxPackStart hboxpanel button3 PackRepel 0

  boxPackStart vbox delim1 PackNatural 5

  area ← drawingAreaNew
  --let snce' a b = execEC $ snce $ (Config a b)
  boxPackStart vbox area PackGrow 20
  --onClicked button1 (set button2 [buttonLabel := "Mdaaa123"])
  --onClicked button2 (set button1 [buttonLabel := "Some text"])

  let drawPic x = updateCanvas (toRenderable $ execEC $ x) area >> return ()

  onClicked button1 $ drawPic $ snce $ Config 2.8 0.5
  onClicked button2 $ drawPic $ visu $ Config 2.6 0.8
  onClicked button3 $ drawPic $ bifu

  boxPackStart vbox delim2 PackNatural 5
  boxPackStart vbox quitButton PackNatural 0

  widgetShowAll window
  mainGUI
