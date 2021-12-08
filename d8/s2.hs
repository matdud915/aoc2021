{-# LANGUAGE OverloadedStrings #-}

import Data.Functor ((<&>))
import Data.Text (pack, splitOn, unpack)
import Data.List (delete, group, sort, nub)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

getInput :: IO [String]
getInput = readFile "input.txt" <&> lines

parseInput :: [String] -> [[String]]
parseInput = map (map unpack . splitOn " | " . pack)

subtractStr :: String -> String -> String
subtractStr = foldl (flip delete)

getDecodeMap :: String -> M.Map String String
getDecodeMap codeString = 
  let
    codedDigits = words codeString
    one = head $ filter (\x -> length x == 2) codedDigits
    four = head $ filter (\x -> length x == 4) codedDigits
    seven = head $ filter (\x -> length x == 3) codedDigits
    eight = head $ filter (\x -> length x == 7) codedDigits
    flatCodedDigits = sort $ concat codedDigits
    eSegment = [head $ head $ filter (\x -> length x == 4) $ group flatCodedDigits]
    bSegment = [head $ head $ filter (\x -> length x == 6) $ group flatCodedDigits]
    fSegment = [head $ head $ filter (\x -> length x == 9) $ group flatCodedDigits]
    aSegment = seven `subtractStr` one
    nine = eight `subtractStr` eSegment
    dSegment = (four `subtractStr` one) `subtractStr` bSegment
    cSegment = four `subtractStr` bSegment `subtractStr` fSegment `subtractStr` dSegment
    gSegment = eight `subtractStr` aSegment `subtractStr` bSegment `subtractStr` cSegment `subtractStr` dSegment `subtractStr` eSegment `subtractStr` fSegment
    zero = concat [aSegment, bSegment, cSegment, eSegment, fSegment, gSegment]
    two = concat [aSegment, cSegment, dSegment, eSegment, gSegment]
    three = concat [aSegment, cSegment, dSegment, fSegment, gSegment]
    five = concat [aSegment, bSegment, dSegment, fSegment, gSegment]
    six = concat [aSegment, bSegment, dSegment, eSegment, fSegment, gSegment]
  in
    M.fromList
      [
        (sort zero, "0"),
        (sort one, "1"),
        (sort two, "2"),
        (sort three, "3"),
        (sort four, "4"),
        (sort five, "5"),
        (sort six, "6"),
        (sort seven, "7"),
        (sort eight, "8"),
        (sort nine, "9")
      ]

decode :: M.Map String String -> String -> Int
decode m c = read $ f $ words c
  where 
    f :: [String] -> String
    f (s:ss) = fromMaybe "d" (M.lookup (sort s) m) ++ f ss
    f [] = []

solve :: [String] -> Int
solve str = decode decodeMap code
  where 
    decodeMap = getDecodeMap $ head str
    code = str !! 1

main = do
  input <- getInput
  let parsedInput = parseInput input
  print $ sum $ map solve parsedInput