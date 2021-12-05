{-# LANGUAGE OverloadedStrings #-}

import Data.List (group, sort)
import Data.Text (splitOn, unpack, pack)

data Point = Point Int Int deriving (Show, Eq, Ord)

makePoint :: String -> Point
makePoint str = 
  let
    coords = splitOn "," (pack str)
    toInt x = read x :: Int
  in
    Point (toInt $ unpack $ coords !! 0) (toInt $ unpack $ coords !! 1)

getInput :: IO [String]
getInput = do
  content <- readFile "input.txt"
  return $ lines content

parseInput :: [String] -> [(Point, Point)]
parseInput input =
  let
    textPoints = map (splitOn " -> " . pack) input
    strPoints = map (map unpack) textPoints
    makePair x = (makePoint $ x !! 0, makePoint $ x !! 1)
  in
    map makePair strPoints

horizontalOrVerticalPredicate :: (Point, Point) -> Bool
horizontalOrVerticalPredicate (Point x1 y1, Point x2 y2) = x1 == x2 || y1 == y2


makeLine :: (Point, Point) -> [Point]
makeLine (Point x1 y1, p2@(Point x2 y2))
  | x1 < x2 && y1 < y2 = Point x1 y1 : makeLine (Point (x1 + 1) (y1 + 1), p2)
  | x1 > x2 && y1 < y2 = Point x1 y1 : makeLine (Point (x1 - 1) (y1 + 1), p2)
  | x1 < x2 && y1 > y2 = Point x1 y1 : makeLine (Point (x1 + 1) (y1 - 1), p2)
  | x1 > x2 && y1 > y2 = Point x1 y1 : makeLine (Point (x1 - 1) (y1 - 1), p2)
  | x1 < x2 = Point x1 y1 : makeLine (Point (x1 + 1) y1, p2)
  | x1 > x2 = Point x1 y1 : makeLine (Point (x1 - 1) y1, p2)
  | y1 < y2 = Point x1 y1 : makeLine (Point x1 (y1 + 1), p2)
  | y1 > y2 = Point x1 y1 : makeLine (Point x1 (y1 - 1), p2)
  | otherwise = [Point x1 y1]

countIntersections :: [Point] -> Int
countIntersections ps =
  let
    groups = group $ sort ps
    moreThanOneAccumulator :: Int -> [Point] -> Int
    moreThanOneAccumulator acc x = if length x > 1 then acc + 1 else acc
    groupsWithMoreThanOneElem = foldl moreThanOneAccumulator 0 groups
  in
    groupsWithMoreThanOneElem

main = do
  input <- getInput
  let parsedInput = parseInput input
  let filtered = filter horizontalOrVerticalPredicate parsedInput
  let linePoints = concatMap makeLine filtered
  print $ countIntersections linePoints
  let linePointsFull = concatMap makeLine parsedInput
  print $ countIntersections linePointsFull

