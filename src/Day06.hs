module Day06 where

import GHC.Show (show)

maxTimes = [59, 68, 82, 74]

distances = [543, 1020, 1664, 1022]

input :: [(Int, Int)]
input = zip maxTimes distances

waysToWin :: Int -> Int -> Int
waysToWin t d = length $ filter (wins d t) [0 .. t]
  where
    wins d mt t = d < t * (mt - t)

part1 :: [(Int, Int)] -> Int
part1 = product . map (uncurry waysToWin)

parse :: ([Int], [Int]) -> (Int, Int)
parse (x, y) = (parse' x, parse' y)
  where
    parse' = (read :: String -> Int) . foldl1 (++) . map show

part2 :: [(Int, Int)] -> Int
part2 = uncurry waysToWin . parse . unzip
