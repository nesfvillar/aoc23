module Day01 where

import Data.Char

input = lines <$> readFile "src/input/day01.txt"

input' = ["1abc2", "pqr3stu8vwx", "a1b2c3d4e5f", "treb7uchet"]

firstDigit :: String -> Char
firstDigit = head . filter isDigit

lastDigit :: String -> Char
lastDigit = firstDigit . reverse

calibrationData :: String -> Int
calibrationData s = (read :: String -> Int) [firstDigit s, lastDigit s]

part1 :: [String] -> Int
part1 = sum . map calibrationData
