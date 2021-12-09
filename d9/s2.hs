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
toIndexed2dMap list = M.fromList $ toIndexed2dMap' 0 0 list
  where
    toIndexed2dMap' :: Int -> Int -> [[Int]] -> [((Int,Int), Int)] 
    toIndexed2dMap' y x ((n:ns):rest) = ((y, x), n) : toIndexed2dMap' y (x+1) (ns:rest)
    toIndexed2dMap' y x ([]:rest) = toIndexed2dMap' (y+1) 0 rest
    toIndexed2dMap' _ _ [] = []

lookupOrHigh :: (Int, Int) -> M.Map (Int, Int) Int -> Int
lookupOrHigh k m = fromMaybe 9 $ M.lookup k m

lookupNeighbours :: (Int, Int) -> M.Map (Int, Int) Int -> [Int]
lookupNeighbours (y, x) m = [
    lookupOrHigh (y - 1, x) m,
    lookupOrHigh (y + 1, x) m,
    lookupOrHigh (y , x - 1) m,
    lookupOrHigh (y , x + 1) m
  ]

getLowPoints :: M.Map (Int, Int) Int -> M.Map (Int, Int) Int
getLowPoints m = getLowPoints' (M.keys m) M.empty
  where 
    predicate :: Int -> [Int] -> Bool
    predicate x n = all (\e -> e > x) n
    getLowPoints' (k:ks) m' = if predicate (lookupOrHigh k m) (lookupNeighbours k m) 
      then getLowPoints' ks (M.insert k (lookupOrHigh k m) m')
      else getLowPoints' ks m'
    getLowPoints' [] m' = m'

getBasin :: (Int, Int) -> M.Map (Int, Int) Int -> Int
getBasin lp m = snd $ getBasin' lp (m, 0)
  where
    getBasin' :: (Int, Int) -> (M.Map (Int, Int) Int, Int) -> (M.Map (Int, Int) Int, Int)
    getBasin' lp'@(y, x) (m', s) =
      let
        testPoint = lookupOrHigh lp' m'
      in
        if testPoint == 9
          then (m', s)
          else (getBasin' (y + 1, x) . getBasin' (y - 1, x) . getBasin' (y, x - 1) . getBasin' (y, x + 1)) (M.delete lp' m', s + 1)

main = do
  input <- getInput
  let wholeMap =  toIndexed2dMap $ parseInput input
  let lowPoints = getLowPoints wholeMap
  print $ product $ take 3 $ reverse $ sort $ map (`getBasin` wholeMap) (M.keys lowPoints)
