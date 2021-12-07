{-# LANGUAGE OverloadedStrings #-}
import Data.Text (splitOn, unpack, pack)

getInput :: IO String
getInput = do
  content <- readFile "input.txt"
  return $ head (lines content)

parseInput :: String -> [Int]
parseInput input =
  let 
    splitted = splitOn "," (pack input)
    strSplitted = map unpack splitted
  in
    map (read :: String -> Int) strSplitted

getDistance :: Int -> Int -> Int
getDistance a b = abs (a - b)

getConsecutiveDistance :: Int -> Int -> Int
getConsecutiveDistance a b = (distance * (distance + 1)) `div` 2
  where distance = getDistance a b

solve :: (Int, Int) -> [Int] -> [Int]
solve (l, r) xs
  | l <= r = sum (map (getDistance l) xs) : solve (l+1, r) xs
  | otherwise = []

solve2 :: (Int, Int) -> [Int] -> [Int]
solve2 (l, r) xs
  | l <= r = sum (map (getConsecutiveDistance l) xs) : solve2 (l+1, r) xs
  | otherwise = []

main = do
  input <- getInput
  let parsedInput = parseInput input
  print $ minimum $ solve2 (minimum parsedInput, maximum parsedInput) parsedInput
