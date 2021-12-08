{-# LANGUAGE OverloadedStrings #-}

import Data.Functor ((<&>))
import Data.Text (pack, splitOn, unpack)

getInput :: IO [String]
getInput = readFile "input.txt" <&> lines

parseInput :: [String] -> [String]
parseInput = map unpack . concatMap (splitOn " | " . pack)

solve :: [String] -> Int
solve (_:x:xs) =
  length (map length $ filter fitUniqueLength (words x)) + solve xs
  where
    fitUniqueLength s = any (\y -> length s == y) [2, 3, 4, 7]
solve [] = 0

main = do
  input <- getInput
  print $ solve $ parseInput input
