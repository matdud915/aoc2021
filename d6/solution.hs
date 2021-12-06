{-# LANGUAGE OverloadedStrings #-}
import Data.Text (splitOn, unpack, pack)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

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
    map (\x -> read x :: Int) strSplitted

simulateLanternfishes :: Int -> [Int] -> [Int]
simulateLanternfishes 0 fs = fs
simulateLanternfishes days fs = simulateLanternfishes (days - 1) (foldl reducer [] fs)
  where
    reducer acc f 
      | f == 0 = 6:8:acc
      | otherwise = (f - 1):acc

fillInitial :: [Int] -> M.Map Int Int
fillInitial xs = fRec xs (M.empty :: M.Map Int Int)
  where 
    fRec (x:xs) mp = case M.lookup x mp of
      Just v -> fRec xs (M.insert x (v + 1) mp)
      Nothing -> fRec xs (M.insert x 1 mp)
    fRec [] mp = mp

simulateWithMap :: Int -> M.Map Int Int -> M.Map Int Int
simulateWithMap 0 mp = mp
simulateWithMap days mp =
  let
    newGenerationCount = fromMaybe 0 (M.lookup 0 mp)
    atSeven = fromMaybe 0 (M.lookup 7 mp)
    atZero = fromMaybe 0 (M.lookup 0 mp)
    mapKeysFn k = if k == 0 then 6 else k - 1
    newMap = M.mapKeys mapKeysFn mp
    newMapWithNewGeneration = 
      if newGenerationCount > 0
        then (M.insert 6 (atSeven + atZero) . M.insert 8 newGenerationCount) newMap
        else newMap
  in simulateWithMap (days - 1) newMapWithNewGeneration

main = do
  input <- getInput
  let parsedInput = parseInput input
  let initialMap = fillInitial parsedInput
  print $ sum $ simulateWithMap 256 initialMap


