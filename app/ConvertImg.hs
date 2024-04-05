{-
-- EPITECH PROJECT, 2024
-- B-PDG-300-COT-3-1-PDGRUSH1-sylvanus.boni
-- File description:
-- wolfram.hs
-}

module ConvertImg where

import Codec.Picture
import Data.Word

unpackPixelRGB8 :: PixelRGB8 -> (Word8, Word8, Word8)
unpackPixelRGB8 (PixelRGB8 r g b) = (r, g, b)

getCoords :: Image PixelRGB8 -> [(Int, Int)]
getCoords img = [(x, y) | x <- [0..((imageWidth img) - 1)],
        y <- [0..((imageHeight img) - 1)]]

getCoordsAlt :: Int -> Int -> Int -> Int -> [(Int, Int)]
getCoordsAlt x y width height
    | x == width && y == height = [(x, y)]
    | x < width = [(x, y)] ++ getCoordsAlt (x + 1) y width height
    | x == width = [(x, y)] ++ getCoordsAlt 0 (y + 1) width height
    | otherwise = []

getRGB :: [(Int, Int)] -> Image PixelRGB8 -> [(Word8, Word8, Word8)]
getRGB [] _ = []
getRGB (l : ls) image = 
    [unpackPixelRGB8 (pixelAt image (fst l) (snd l))] ++ getRGB ls image

writePixels :: [(Int, Int)] -> [(Word8, Word8, Word8)] -> IO()
writePixels [] _ = putStr ""
writePixels (l : ls) (x : xs) =
    appendFile "input" ((show l) ++ " " ++ (show x) ++ "\n")
    >> writePixels ls xs
