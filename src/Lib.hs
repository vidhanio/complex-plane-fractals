module Lib (Frame (..), ZResult (..), calculateZ) where

import Data.Complex (Complex (..), magnitude)

data Frame = Frame
    { width :: Int
    , height :: Int
    , offset :: Complex Double
    , scale :: Double
    , maxIterations :: Int
    , algorithm :: Complex Double -> Complex Double -> Complex Double
    }

scaleTo :: Fractional a => (a, a) -> (a, a) -> a -> a
scaleTo (min1, max1) (min2, max2) x =
    min2 + (x - min1) * (max2 - min2) / (max1 - min1)

calculateC :: Frame -> (Int, Int) -> Complex Double
calculateC Frame{width, height, offset = (ro :+ io), scale} (x, y) =
    real :+ imaginary
  where
    real = scaleTo (0, fromIntegral width) (-2.5, 1) (fromIntegral x) / scale + ro
    imaginary = scaleTo (0, fromIntegral height) (-1, 1) (fromIntegral y) / scale + io

data ZResult
    = Escaped Int (Complex Double)
    | Present

calculateZ :: Frame -> (Int, Int) -> ZResult
calculateZ frame@Frame{algorithm, maxIterations} coords =
    calculateZ' 0 (0 :+ 0)
  where
    c = calculateC frame coords

    calculateZ' i z
        | i == maxIterations = Present
        | magnitude z > 2 = Escaped i z
        | otherwise = calculateZ' (i + 1) (algorithm c z)
