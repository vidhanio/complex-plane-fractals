{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}

module Main (main) where

import Data.Complex (Complex (..))
import Data.Function (on, (&))
import Lib (Frame (..), ZResult (..), calculateZ)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import Text.Read (readEither)

main :: IO ()
main = getArgs >>= flip parseArgs defaultFrame >>= run
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

parseArgs :: [String] -> Frame -> IO Frame
parseArgs args frame =
    case args of
        ["help"] -> printHelp >> exitSuccess
        "-w" : w : xs -> readOrExit "width" w >>= parseArgs xs . (\width -> frame{width})
        "-h" : h : xs -> readOrExit "height" h >>= parseArgs xs . (\height -> frame{height})
        "-i" : i : xs -> readOrExit "maxIterations" i >>= parseArgs xs . (\maxIterations -> frame{maxIterations})
        "-o" : o : xs -> readOrExit "offset" o >>= parseArgs xs . (\offset -> frame{offset})
        "-s" : s : xs -> readOrExit "scale" s >>= parseArgs xs . (\scale -> frame{scale})
        "-a" : "mandelbrot" : _ -> pure frame{algorithm = mandelbrot}
        "-a" : "burningShip" : _ -> pure frame{algorithm = burningShip}
        "-a" : a : _ -> unrecognizedAndExit "algorithm" a
        f@['-', _] : _ -> unrecognizedAndExit "flag" f
        cmd : _ -> unrecognizedAndExit "command" cmd
        [] -> pure frame
  where
    readOrExit :: (Read a) => String -> String -> IO a
    readOrExit flag s =
        readEither s
            & either (const $ hPutStrLn stderr flag >> exitFailure) pure

mandelbrot :: Complex Double -> Complex Double -> Complex Double
mandelbrot c z =
    z * z + c

burningShip :: Complex Double -> Complex Double -> Complex Double
burningShip c (a :+ b) =
    mandelbrot c (abs a :+ abs b)

run :: Frame -> IO ()
run frame = printHeader frame >> printPixels frame

printHelp :: IO ()
printHelp = do
    progName <- getProgName

    hPutStrLn stderr $ unwords ["Usage:", progName, options]
  where
    options = "[help] [-w width] [-h height] [-i maxIterations] [-o offset] [-s scale] [-a algorithm]"

printHeader :: Frame -> IO ()
printHeader frame = do
    putStrLn "P3"
    putStrLn . unwords $ map show [width frame, height frame]
    putStrLn "255"

unrecognizedAndExit :: String -> String -> IO a
unrecognizedAndExit what s = do
    hPutStrLn stderr (unwords ["Unrecognised", what <> ":", s]) >> printHelp >> exitFailure

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
