-- Main.hs

import Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as UI
import Control.Monad
import Mandelbrot
main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do
    return window # set title "Mandelbrot Set"
    canvas <- UI.canvas
        # set UI.width 800
        # set UI.height 600
        # set style [("border", "1px solid black")]
    getBody window #+ [element canvas]
    on UI.click canvas $ \_ -> renderMandelbrot canvas


renderMandelbrot :: Element -> UI ()
renderMandelbrot canvas = do
    -- Set up canvas context
    context <- liftIO $ getCanvasContext canvas
    let width = 800
        height = 600
    liftIO $ setCanvasSize canvas width height

    -- Define Mandelbrot parameters
    let maxIterations = 100
        xMin = -2.0
        xMax = 1.0
        yMin = -1.5
        yMax = 1.5

    -- Iterate over pixels and compute Mandelbrot set
    forM_ [0 .. width - 1] $ \px -> do
        forM_ [0 .. height - 1] $ \py -> do
            let x = xMin + (fromIntegral px / fromIntegral width) * (xMax - xMin)
                y = yMin + (fromIntegral py / fromIntegral height) * (yMax - yMin)
                iterations = mandelbrotIterations x y maxIterations
                color = getColor iterations  -- Get the color based on iterations
            liftIO $ setCanvasColor context px py color



-- Utility function to set canvas color at a specific pixel
setCanvasColor :: UI.Canvas -> Int -> Int -> String -> UI ()
setCanvasColor canvas x y color = do
    -- Set the fill color
    UI.fillStyle color
    -- Draw a filled rectangle (1x1 pixel) at the specified position
    UI.fillRect (fromIntegral x) (fromIntegral y) 1 1 canvas

-- Utility function to set canvas size
setCanvasSize :: Element -> Int -> Int -> IO ()
setCanvasSize canvas w h = do
    -- Set the canvas dimensions
    element canvas # set UI.width w
                   # set UI.height h
