{-# LANGUAGE OverloadedStrings #-}

import Data.Functor ((<&>))
import Data.Text (pack, splitOn, unpack)
import Data.Maybe (fromMaybe)
import Data.List (sort)
import qualified Data.Map as M

getInput :: IO [String]
getInput = readFile "input.txt" <&> lines

parseInput :: [String] -> [[Int]]
parseInput = map getDigits
  where
    getDigits :: String -> [Int] 
    getDigits = map (\x -> read [x])

toIndexed2dMap :: [[Int]] -> M.Map (Int, Int) Int
toIndexed2dMap list = M.fromList $ fRec' 0 0 list
  where
    fRec' :: Int -> Int -> [[Int]] -> [((Int,Int), Int)] 
    fRec' y x ((n:ns):rest) = ((y, x), n) : fRec' y (x+1) (ns:rest)
    fRec' y x ([]:rest) = fRec' (y+1) 0 rest
    fRec' _ _ [] = []

high :: Int
high = 9

lookupOrHigh :: (Int, Int) -> M.Map (Int, Int) Int -> Int
lookupOrHigh k m = fromMaybe high $ M.lookup k m

getNeighbourIndexes :: (Int, Int) -> [(Int, Int)]
getNeighbourIndexes (y, x) = [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]

lookupNeighbours :: (Int, Int) -> M.Map (Int, Int) Int -> [Int]
lookupNeighbours c m = map (`lookupOrHigh` m) $ getNeighbourIndexes c

getLowPoints :: M.Map (Int, Int) Int -> M.Map (Int, Int) Int
getLowPoints m = fRec' (M.keys m) M.empty
  where 
    predicate :: Int -> [Int] -> Bool
    predicate x n = all (> x) n
    fRec' (k:ks) m' = if predicate (lookupOrHigh k m) (lookupNeighbours k m) 
      then fRec' ks $ M.insert k (lookupOrHigh k m) m'
      else fRec' ks m'
    fRec' [] m' = m'

getBasin :: (Int, Int) -> M.Map (Int, Int) Int -> Int
getBasin lp m = snd $ getBasin' lp (m, 0)
  where
    getBasin' :: (Int, Int) -> (M.Map (Int, Int) Int, Int) -> (M.Map (Int, Int) Int, Int)
    getBasin' lp'@(y, x) (m', s) =
      if lookupOrHigh lp' m' == high
        then (m', s)
        else foldl (flip getBasin') (M.delete lp' m', s + 1) (getNeighbourIndexes lp')

main = do
  input <- getInput
  let wholeMap = toIndexed2dMap $ parseInput input
  let lowPoints = getLowPoints wholeMap
  print $ product $ take 3 $ reverse $ sort $ map (`getBasin` wholeMap) (M.keys lowPoints)
