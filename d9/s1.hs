{-# LANGUAGE OverloadedStrings #-}

import Data.Functor ((<&>))
import Data.Text (pack, splitOn, unpack)
import Data.Maybe (fromMaybe)
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

solve :: M.Map (Int, Int) Int -> [Int]
solve m = solve' $ M.keys m
  where 
    predicate :: Int -> [Int] -> Bool
    predicate x n = all (\e -> e > x) n
    solve' (k:ks) = if predicate (lookupOrHigh k m) (lookupNeighbours k m) then (lookupOrHigh k m) : solve' ks else solve' ks
    solve' [] = []

calcRiskLevel :: [Int] -> Int
calcRiskLevel = sum . map (\x -> x + 1)

main = do
  input <- getInput
  print $ calcRiskLevel $ solve $ toIndexed2dMap $ parseInput input
