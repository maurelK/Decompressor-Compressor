{-
-- EPITECH PROJECT, 2024
-- B-PDG-300-COT-3-1-PDGRUSH1-sylvanus.boni
-- File description:
-- wolfram.hs
-}

module ModifImg where

import Codec.Picture
import Data.Word

modifyRGB :: PixelRGB8 -> (Word8, Word8, Word8) -> PixelRGB8
modifyRGB (PixelRGB8 r g b) (x, y, z) = (PixelRGB8 x y z)

myThird :: (a, b, c) -> c
myThird (_, _, c) = c

myFt :: (a, b, c) -> a
myFt (a, _, _) = a

mySd :: (a, b, c) -> b
mySd (_, b, _) = b

updatePixel :: PixelRGB8 -> Int -> Int -> Image PixelRGB8 -> Image PixelRGB8
updatePixel newPixel x y image = generateImage gen newWidth newHeight
  where
    (width, height) = (imageWidth image, imageHeight image)
    gen u v
      | u == x && v == y = newPixel
      | otherwise = pixelAt image u v
    newWidth = width
    newHeight = height

kImage :: Image PixelRGB8 -> [((Int, Int), (Float, Float, Float), Int)] -> [((Int, Int), (Float, Float, Float))] -> Image PixelRGB8
kImage image [] _ = image
kImage image (((x, y), a, n) : ls) xs =
  let (r, g, b) = (fromIntegral (floor (myFt (snd (xs !! n)))), fromIntegral (floor (mySd (snd (xs !! n)))), fromIntegral (floor (myThird (snd (xs !! n)))))
      newPixel = modifyRGB (pixelAt image x y) (fromIntegral r, fromIntegral g, fromIntegral b)
  in kImage (updatePixel newPixel x y image) ls xs

writeImage :: FilePath -> Image PixelRGB8 -> IO ()
writeImage filePath image = writePng filePath image
