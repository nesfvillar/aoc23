module Day01 where

import Data.Char (isDigit)
import Data.List (isPrefixOf)

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

parseWord :: String -> String
parseWord = parseWord' []
  where
    parseWord' acc [] = acc
    parseWord' acc w = parseWord' (acc ++ initialDigit w) (tail w)
    initialDigit w
      | isDigit (head w) = [head w]
      | "one" `isPrefixOf` w = "1"
      | "two" `isPrefixOf` w = "2"
      | "three" `isPrefixOf` w = "3"
      | "four" `isPrefixOf` w = "4"
      | "five" `isPrefixOf` w = "5"
      | "six" `isPrefixOf` w = "6"
      | "seven" `isPrefixOf` w = "7"
      | "eight" `isPrefixOf` w = "8"
      | "nine" `isPrefixOf` w = "9"
      | otherwise = ""

part2 :: [String] -> Int
part2 = part1 . map parseWord
