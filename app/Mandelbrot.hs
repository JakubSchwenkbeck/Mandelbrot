--| Mandelbrot module handles all the calculations with the Complex numbers and the pixels
module Mandelbrot 
    ( Complex (..)
    , Steps
    , ViewWindow (..)
    , PictureCoords (..)
    , PictureSize (..)
    , createImage
    , createImageInParallel
    , project
    , zoomTo
    ) where

import Codec.Picture (PixelRGB8(..), Image, generateImage, writePng)
--import Data.Array.Repa as Repa hiding ((++))
import Data.Functor.Identity

--| simple Complex Number struct
data Complex =
    C { re :: Double
      , im :: Double
      } deriving (Eq)

--| Simple Window Struct
data ViewWindow =
    View { upperLeft  :: Complex
         , lowerRight :: Complex
         } deriving (Show)

--| init Coords ad size
data PictureCoords = Coords { x :: Int, y :: Int }
data PictureSize   = Size   { width :: Int, height :: Int }

type Steps = Int
type RGB   = (Int, Int, Int)

--| Using parrallel processing, this creates the image to display
createImageInParallel :: ViewWindow -> Steps -> PictureSize -> String -> IO()
createImageInParallel view maxSteps resolution imagePath = do
  let image = mandelbrotParallel maxSteps view resolution
  writePng imagePath image
  
--| Non parallel version of creating the Image
createImage :: ViewWindow -> Steps -> PictureSize -> String -> IO()
createImage view maxSteps resolution imagePath = do
  let image = mandelbrotImage maxSteps view resolution
  writePng imagePath image

-- | calculates the mandelbrot-set as an colored image
mandelbrotImage :: Steps -> ViewWindow -> PictureSize -> Image PixelRGB8
mandelbrotImage maxSteps vw sz@(Size w h) = generateImage calcPixel w h
    where calcPixel x' y' = color maxSteps $ mandelbrotIter maxSteps $ project vw sz (Coords x' y')

color :: Steps -> Steps -> PixelRGB8
color maxSteps = rgbToPixelRGB8 . colorRGB maxSteps
rgbToPixelRGB8 :: RGB -> PixelRGB8
rgbToPixelRGB8 (r, g, b) = PixelRGB8 (fromIntegral r) (fromIntegral g) (fromIntegral b)


--| Generate the mandelbrot set as a list of lists
mandelbrotArray :: Steps -> ViewWindow -> PictureSize -> [[RGB]]
mandelbrotArray maxSteps vw sz@(Size w h) =
    [[colorRGB maxSteps (mandelbrotIter maxSteps (project vw sz (Coords x' y'))) | x' <- [0..w-1]] | y' <- [0..h-1]]

--| Generate the image from the list of lists
mandelbrotParallel :: Steps -> ViewWindow -> PictureSize -> Image PixelRGB8
mandelbrotParallel maxSteps vw sz@(Size w h) = generateImage getPixel w h
    where
        pixelArray = mandelbrotArray maxSteps vw sz
        getPixel x' y' = rgbToPixelRGB8 (pixelArray !! y' !! x')


-- | translates the steps taken till the sequence got out-of-bounds into a RGB value for a color using a very basic approach of just bit-masking (notice: in the later stages the PixelRGB8 structure will "mod" to 8-bits by default)
colorRGB :: Steps -> Steps -> RGB
colorRGB maxSteps steps
    | steps >= maxSteps = (0, 0, 0)
    | otherwise         = (sr, sg, sb)
    where sr = steps
          sg = 64 * (steps `div` 256)
          sb = 16 * (steps `div` 65536)



-- | translates a picture-coords into the ViewWindow
project :: ViewWindow -> PictureSize -> PictureCoords -> Complex
project (View ul lr) sz c = ul + (C (w*x') (h*y'))
    where (C w h) = lr - ul
          x'      = fromIntegral (x c) / fromIntegral (width sz)
          y'      = fromIntegral (y c) / fromIntegral (height sz)

viewWidth :: ViewWindow -> Double
viewWidth v = re $ lowerRight v - upperLeft v

viewHeight :: ViewWindow -> Double
viewHeight v = im $ lowerRight v - upperLeft v

zoomTo :: Double -> Complex -> ViewWindow -> ViewWindow
zoomTo z c v = View (c-d) (c+d)
    where d = C (w/2) (h/2)
          w = (1/z) * viewWidth v
          h = (1/z) * viewHeight v

-- | processes - based on a maximum step count and a starting point -
--   the numbers or steps it takes a point using the iteration-rule
--   z' = z*z + c - to escape the bound region
mandelbrotIter :: Steps -> Complex -> Steps
mandelbrotIter maxSteps c = runIter 0 c
    where runIter steps z =
            if steps >= maxSteps || escapes z
                then steps
                else runIter (steps+1) (iter z)
          iter z = z*z + c

-- | does a point in the complex plane escape to infinity
--   (of course using mandelbrots iteration rule z' = z*z + c)
escapes :: Complex -> Bool
escapes = (>= 4) . len2

len2 :: Complex -> Double
len2 (C r i) = r*r + i*i

-- | Implement Complex as Num represented as a complex number in the
--   most obvious way - notice that signum was choosen to fullfill
--   abs x * signum x == x
instance Num Complex where
    fromInteger i = C (fromInteger i) 0
    (C r i) + (C r' i') = C (r+r') (i+i')
    (C r i) - (C r' i') = C (r-r') (i-i')
    (C r i) * (C r' i') = C (r*r' - i*i') (r*i' + i*r')
    negate (C r i)      = C (-r) (-i)
    abs c               = C (sqrt $ len2 c) 0
    signum c@(C r i)    = C (r / l2) (i / l2)
        where l2        = len2 c

instance Show Complex where 
  show (C r i) = show r ++ " + " ++ show i ++ "I"
