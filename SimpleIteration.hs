{-# LANGUAGE FlexibleContexts, UnicodeSyntax #-}

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

logmapfoo :: Double → Double → Double
logmapfoo r x = r * x * (1 - x)
logmap  = iterate . logmapfoo

main = toFile def "example1_big.png" $ do
    layout_title .= "Amplitude Modulation"
    setColors [opaque blue, opaque pink, opaque red, opaque green,  opaque maroon]
    let rs = [0.5, 1.5, 2.5, 3.1, 3.44]
    mapM_ (\rs → plot (line
                       ("Convergence: " ++ show rs)
                       [take 50 $ zip [(0 :: Double)..] $ logmap rs 0.5])) rs
