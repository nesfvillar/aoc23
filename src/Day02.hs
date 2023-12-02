module Day02 where

import Numeric.Natural

input = map parseInput . lines <$> readFile "src/input/day02.txt"

input' =
  map
    parseInput
    [ "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
      "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
      "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
      "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
      "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    ]

data Draw = Draw {red :: Natural, green :: Natural, blue :: Natural}

data Game = Game {id :: Int, draws :: [Draw]}

parseInput :: String -> Game
parseInput = undefined

maxDraw :: Draw -> Draw -> Draw
maxDraw a b = Draw {red = maxRed a b, green = maxGreen a b, blue = maxBlue a b}
  where
    maxRed a b = max (red a) (red b)
    maxGreen a b = max (green a) (green b)
    maxBlue a b = max (blue a) (blue b)

isPossible :: Draw -> Bool
isPossible d = red d <= 12 && green d <= 13 && blue d <= 14

part1 :: [Game] -> Int
part1 = sum . map (fromEnum . isPossible . foldr1 maxDraw . draws)

part2 :: [String] -> Int
part2 = undefined
