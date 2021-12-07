{-# LANGUAGE OverloadedStrings #-}

import Data.Functor ((<&>))
import Data.Text (pack, splitOn, unpack)

getInput :: IO String
getInput = readFile "input.txt" <&> (head . lines)

parseInput :: String -> [Int]
parseInput = map ((read :: String -> Int) . unpack) . splitOn "," . pack

getDistance :: Int -> Int -> Int
getDistance a b = abs (a - b)

getIncreasingDistance :: Int -> Int -> Int
getIncreasingDistance a b = (distance * (distance + 1)) `div` 2
  where
    distance = getDistance a b

solve :: (Int, Int) -> [Int] -> [Int]
solve (l, r) xs
  | l <= r = (sum . map (getDistance l)) xs : solve (l + 1, r) xs
  | otherwise = []

solve2 :: (Int, Int) -> [Int] -> [Int]
solve2 (l, r) xs
  | l <= r = (sum . map (getIncreasingDistance l)) xs : solve2 (l + 1, r) xs
  | otherwise = []

main = do
  input <- getInput
  let parsedInput = parseInput input
  print $ minimum $ solve (minimum parsedInput, maximum parsedInput) parsedInput
  print $
    minimum $ solve2 (minimum parsedInput, maximum parsedInput) parsedInput
