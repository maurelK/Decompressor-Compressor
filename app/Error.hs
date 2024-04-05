{-
-- EPITECH PROJECT, 2023
-- B-PDG-300-COT-3-1-PDGRUSH1-lawo.lawo
-- File description:
-- wolfram.hs
-}

module Error where

import Data.Char (isDigit)
import System.Exit

errors :: IO ()
errors = putStrLn "USAGE: ./imageCompressor -n N -l L -f F\n\n\
    \tN\tnumber of colors in the final image\n\
    \tL\tconvergence limit\n\
    \tF\tpath to the file containing the colors of the pixels"
    >> exitWith (ExitFailure(84))

isNum :: String -> Bool
isNum [] = False
isNum a = if all isDigit a
        then True
        else False

isDouble :: String -> Int -> Bool -> Bool
isDouble [] _ _ = True
isDouble ('.':s) i False = if (i>=1) then isDouble s (i+1) True else False
isDouble (x : xs) i ch = if isDigit x then isDouble xs (i + 1) ch else False

checkArgs :: [String] -> IO Bool
checkArgs [] = return True
checkArgs ("-n":b:ls) = if isNum b 
    then checkArgs ls
    else return False
checkArgs ("-l":d:ls) = if (isDouble d 0 False)
    then checkArgs ls
    else return False
checkArgs ("-f":f:ls) = do
    if (True)
        then checkArgs ls
        else return False
checkArgs (_:ls) = return False
