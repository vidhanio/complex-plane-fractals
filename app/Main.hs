{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}

module Main (main) where

import Data.Complex (Complex (..))
import Data.Function (on, (&))
import Lib (Frame (..), ZResult (..), calculateZ)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

main :: IO ()
main = getArgs >>= flip parse defaultFrame >>= run
  where
    defaultFrame =
        Frame
            { width = 1920
            , height = 1080
            , offset = 0 :+ 0
            , scale = 1
            , maxIterations = 100
            , algorithm = mandelbrot
            }

parse :: [String] -> Frame -> IO Frame
parse args frame =
    case args of
        ["-h"] -> printHelp >> exitSuccess
        "-w" : w : xs -> parseOrExit w >>= parse xs . (\width -> frame{width})
        "-h" : h : xs -> parseOrExit h >>= parse xs . (\height -> frame{height})
        "-i" : i : xs -> parseOrExit i >>= parse xs . (\maxIterations -> frame{maxIterations})
        "-o" : o : xs -> parseOrExit o >>= parse xs . (\offset -> frame{offset})
        "-s" : s : xs -> parseOrExit s >>= parse xs . (\scale -> frame{scale})
        "-a" : "mandelbrot" : _ -> pure frame{algorithm = mandelbrot}
        "-a" : "burningShip" : _ -> pure frame{algorithm = burningShip}
        [] -> pure frame
        _ -> printHelp >> exitFailure
  where
    parseOrExit :: (Read a) => String -> IO a
    parseOrExit s =
        readMaybe s
            & maybe (printHelp >> exitFailure) pure

mandelbrot :: Complex Double -> Complex Double -> Complex Double
mandelbrot c z =
    z * z + c

burningShip :: Complex Double -> Complex Double -> Complex Double
burningShip c (a :+ b) =
    mandelbrot c (abs a :+ abs b)

run :: Frame -> IO ()
run frame = printHeader frame >> printPixels frame

printHelp :: IO ()
printHelp = hPutStrLn stderr "Usage: complex-plane-fractals [-h] [-w width] [-h height] [-i maxIterations] [-o offset] [-s scale] [-a algorithm]"

printHeader :: Frame -> IO ()
printHeader frame = do
    putStrLn "P3"
    putStrLn . unwords $ map show [width frame, height frame]
    putStrLn "255"

printPixels :: Frame -> IO ()
printPixels frame = do
    mapM_ (printPixel frame) pixels
  where
    pixels =
        [0 .. height frame - 1]
            & concatMap (\y -> map (,y) [0 .. width frame - 1])

printPixel :: Frame -> (Int, Int) -> IO ()
printPixel frame = putStrLn . pixelText frame . calculateZ frame

pixelText :: Frame -> ZResult -> String
pixelText frame z =
    case pixelRgb frame z of
        (r, g, b) -> unwords $ map show [r, g, b]

pixelRgb :: Frame -> ZResult -> (Int, Int, Int)
pixelRgb _ Present = (0, 0, 0)
pixelRgb Frame{maxIterations} (Escaped i _) =
    (v, v, v)
  where
    percent :: Double
    percent = ((/) `on` fromIntegral) i maxIterations

    v :: Int
    v = round $ 255 * percent
