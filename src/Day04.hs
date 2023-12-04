module Day04 where

import Numeric.Natural (Natural)

data Card = Card {winningNumbers, numbers :: [Int]}

winners :: Card -> [Int]
winners card = filter (\c -> c `elem` winningNumbers card) $ numbers card

value :: Card -> Natural
value card =
  let n = length (winners card)
   in if n == 0 then 0 else 2 ^ (n - 1)

part1 :: [Card] -> Natural
part1 = sum . map value

part2 :: [Card] -> Int
part2 = undefined
