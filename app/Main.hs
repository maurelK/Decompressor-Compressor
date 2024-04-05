{-
-- EPITECH PROJECT, 2024
-- B-PDG-300-COT-3-1-PDGRUSH1-sylvanus.boni
-- File description:
-- wolfram.hs
-}

module Main (main) where

import Data.List (elemIndex, nub)
import Data.Maybe (fromMaybe)
import System.Environment
import System.Random
import Data.Word
import Error
import Codec.Picture
import ConvertImg
import ModifImg

toPair :: String -> (String, String) -> Bool -> (String, String)
toPair [] p _ = p
toPair (' ' : xs) (a, b) _ = toPair xs (a, b) True
toPair (x : xs) (a, b) True = toPair xs (a, b ++ [x]) True
toPair (x : xs) (a, b) bl = toPair xs (a ++ [x], b) bl

toFloat :: [(String, String)] -> [(String, (Float, Float, Float))]
toFloat [] = []
toFloat ((a, b) : ls) = [(a, (read b))] ++ toFloat ls

bof :: [String] -> [(String, String)]
bof [] = []
bof (x : ls) = [toPair x ("", "") False] ++ bof ls

distance :: (Float, Float, Float) -> (Float, Float, Float) -> Float
distance (x,y,z) (a,b,c) = sqrt ((a-x)^2+(b-y)^2+(c-z)^2)

myScd :: (a, b) -> b
myScd (_, b) = b

myFst :: (a, b) -> a
myFst (a, _) = a

getMin :: (Float, Float, Float) -> [(String, (Float, Float, Float))] -> [Float] -> Int
getMin _ [] ls = fromMaybe 0 (elemIndex (minimum ls) ls)
getMin pixel (x : xs) ls = getMin pixel xs (ls ++ [distance pixel (snd x)])

rngPixs :: [(String, (Float, Float, Float))] -> [(String, (Float, Float, Float))] -> [(String, (Float, Float, Float), Int)]
rngPixs [] _ = []
rngPixs ((a, b) : xs) ls = [(a, b, (getMin b ls []))] ++ rngPixs xs ls

genList :: Int -> Int -> Int -> IO [Int]
genList n minVal maxVal = do
  gen <- getStdGen
  let randomNumbers = take n (randomRs (minVal, maxVal) gen)
  return $ nub randomNumbers

getCentroids :: [(String, (Float, Float, Float))] -> [Int] -> [(String, (Float, Float, Float))]
getCentroids _ [] = []
getCentroids ls (x : xs) = [ls !! x] ++ getCentroids ls xs

computeAverage :: [(String, (Float, Float, Float), Int)] -> Int -> (Float, Float, Float) -> Float -> (Float, Float, Float)
computeAverage [] _ (x, y, z) total = ((x / total), (y / total), (z / total))
computeAverage ((_, (x, y, z), n) : ls) i (tx, ty, tz) total
    | (n == i) = computeAverage ls i (tx + x, ty + y, tz + z) (total + 1)
    | otherwise = computeAverage ls i (tx, ty, tz) total

checkAv :: (Float, Float, Float) -> (Float, Float, Float) -> Float -> Bool
checkAv a b cv = if distance b a <= cv
    then True
    else False

computeLoop :: [(String, (Float, Float, Float), Int)] -> [(String, (Float, Float, Float))] -> Int -> Float -> Int
computeLoop _ [] _ _ = 0
computeLoop xs (l : ls) n convergence
    | checkAv (computeAverage xs n (0, 0, 0) 0.0) (snd l)
                convergence = 1 + computeLoop xs ls (n + 1) convergence
    | otherwise = computeLoop xs ls (n + 1) convergence

newCentroids :: [(String, (Float, Float, Float), Int)] -> [(String, (Float, Float, Float))] -> Int -> [(String, (Float, Float, Float))]
newCentroids _ [] _ = []
newCentroids l ((a,b) : x) n = 
    [(a,(computeAverage l n (0, 0, 0) 0))] ++ newCentroids l x (n+1)

rangePixellls :: [(String, (Float, Float, Float), Int)] -> [(String, (Float, Float, Float))] -> [(String, (Float, Float, Float), Int)]
rangePixellls [] _ = []
rangePixellls ((a, b, _) : xs) ls =
    [(a, b, (getMin b ls []))] ++ rangePixellls xs ls

nextGen :: [(String, (Float, Float, Float), Int)] -> [(String, (Float, Float, Float))] -> Float -> ([(String, (Float, Float, Float), Int)], [(String, (Float, Float, Float))])
nextGen ls xs convergence
    | not (computeLoop ls xs 0 convergence == (length xs)) =
        let lst = newCentroids ls xs 0
        in nextGen (rangePixellls ls lst) lst convergence
    | otherwise = (ls, xs)

output :: [(String, (Float, Float, Float), Int)] -> Int -> IO()
output [] _ = putStr ""
output ((a, (r, g, b), c) : ls) n
    | n == c = putStr (show (read a::(Int,Int))) >>
        putStr (" ") >>
        dispRgb (r, g, b) >>
        output ls n
    | otherwise = output ls n

dispRgb :: (Float, Float, Float) -> IO()
dispRgb (r, g, b) = putStrLn $ "(" ++ show (truncate (roundDb r)) ++ 
    "," ++ show (truncate (roundDb g)) ++ "," ++ 
    show (truncate (roundDb b)) ++ ")"

dispLoop :: [(String, (Float, Float, Float), Int)] -> [(String, (Float, Float, Float))] -> Int -> IO()
dispLoop _ [] _ = putStr ""
dispLoop ls ((_, (r, g, b)) : xs) n = putStrLn "--" >>
    dispRgb (r, g, b) >>
    putStrLn "-" >>
    output ls n >>
    dispLoop ls xs (n + 1)

change :: [(String, (Float, Float, Float), Int)] -> [((Int, Int), (Float, Float, Float), Int)]
change [] = []
change ((a, b, c) : ls) = [((read a), b, c)] ++ change ls

changeBof :: [(String, (Float, Float, Float))] -> [((Int, Int), (Float, Float, Float))]
changeBof [] = []
changeBof ((a, b) : ls) = [((read a), b)] ++ changeBof ls

roundDb :: Float -> Float
roundDb x =
  if (x - fromIntegral (floor x)) >= 0.5
    then fromIntegral (ceiling x)
    else fromIntegral (floor x)

myFindby :: [String] -> String -> String
myFindby (a : b : ls) tofind
    | a == tofind = b
    | otherwise = myFindby ls tofind


myMain :: [String] -> IO()
myMain args = do
    file <- readFile (myFindby args "-f")
    let lst = toFloat (bof (lines file))
    rd <- genList (read (myFindby args "-n")) 0 ((length lst) - 1)
    let c = getCentroids lst rd
    let final = nextGen (rngPixs lst c) c (read (myFindby args "-l"))
    dispLoop (myFst final) (myScd final) 0


{-main :: IO()
main = do
    args <- getArgs
    bl <- (checkArgs args)
    if (not (length args == 6) || not bl)
        then errors else myMain args-}

main :: IO ()
main = do
    eitherImage <- readImage "pdfimage.png"
    case eitherImage of
        Left err -> putStrLn $ "Bad loading: " ++ err
        Right dynamicImage -> do
            let image = convertRGB8 dynamicImage
            args <- getArgs
            let coords = getCoordsAlt 0 0 ((imageWidth image) - 1) ((imageHeight image) - 1)
            writePixels coords (getRGB coords image)
            bl <- (checkArgs args)
            if (not (length args == 6) || not bl)
                then errors else putStr ""
            file <- readFile "input" --(args !! 5)
            let lst = toFloat (bof (lines file))
            rd <- genList (read (myFindby args "-n")) 0 ((length lst) - 1)
            let c = getCentroids lst rd
            let final = nextGen (rngPixs lst c) c (read (myFindby args "-l"))
            dispLoop (myFst final) (myScd final) 0
            let bof = change (myFst final)
            let bof2 = changeBof (myScd final)
            writeImage "result.png" (kImage image bof bof2)
