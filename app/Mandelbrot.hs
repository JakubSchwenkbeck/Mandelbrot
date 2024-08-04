-- Mandelbrot.hs
module Mandelbrot where

import Data.Complex

-- Compute Mandelbrot iterations for a given point (x, y)
mandelbrotIterations :: Double -> Double -> Int -> Int
mandelbrotIterations x y maxIterations = go (0 :+ 0) 0
  where
    go z iter
        | iter >= maxIterations = iter
        | magnitude z > 2.0 = iter
        | otherwise = go (z^2 + (x :+ y)) (iter + 1)

-- Map iterations to colors (you can customize this function)
getColor iterations
    | iterations == maxIterations = "black"  -- Inside the Mandelbrot set
    | otherwise = "orange"  -- Outside the Mandelbrot set
  where
    maxIterations = 100  -- Adjust this value as needed