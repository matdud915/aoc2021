{-# LANGUAGE OverloadedStrings #-}

import Data.Functor ((<&>))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

getInput :: IO [String]
getInput = readFile "input.txt" <&> lines

parseInput :: String -> [Int]
parseInput = map (\x -> read [x] :: Int)

to2dIndexedMap :: [[Int]] -> M.Map (Int, Int) Int
to2dIndexedMap [] = M.empty
to2dIndexedMap (x:xs) = M.fromList $ helper 0 0 x xs
    where
        helper :: Int -> Int -> [Int] -> [[Int]] -> [((Int, Int), Int)]
        helper _ _ [] [] = []
        helper y _ [] (b:bs) = helper (y+1) 0 b bs
        helper y x (a:as) bs = ((y, x), a) : helper y (x+1) as bs

unsafeLookup :: (Int, Int) -> M.Map (Int, Int) Int -> Int
unsafeLookup k m = fromMaybe 0 (M.lookup k m)

getNeighbours :: (Int, Int) -> [(Int, Int)]
getNeighbours (y, x) = [(y+1, x), (y+1, x+1), (y, x+1), (y-1, x+1), (y-1, x), (y-1, x-1), (y, x-1), (y+1, x-1)]

increaseLevel :: (Int, Int) -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int
increaseLevel k m = case M.lookup k m of
    Just x -> M.insert k (x+1) m
    Nothing -> m

increaseLevels :: [(Int, Int)] -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int
increaseLevels [] m = m
increaseLevels (l:ls) m =
    let
        increasedMap = increaseLevel l m
        newChecks = if unsafeLookup l increasedMap == 10 then ls ++ getNeighbours l else ls
    in
        increaseLevels newChecks increasedMap

flash :: M.Map (Int, Int) Int -> M.Map (Int, Int) Int
flash = M.map (\x -> if x >= 10 then 0 else x)

getFlashes :: M.Map (Int, Int) Int -> Int
getFlashes m = sum $ M.elems $ M.map (\x -> if x >= 10 then 1 else 0) m

step :: M.Map (Int, Int) Int -> (Int, M.Map (Int, Int) Int)
step m =
    let
        keys = M.keys m
        increasedMap = increaseLevels keys m
        flashes = getFlashes increasedMap
        flashedMap = flash increasedMap
    in
        (flashes, flashedMap)

getSumOfFlashes :: Int -> M.Map (Int, Int) Int -> Int
getSumOfFlashes 0 m = 0
getSumOfFlashes s m =
    let
        (flashes, newMap) = step m
    in
        flashes + getSumOfFlashes (s - 1) newMap

chunkify :: Int -> [a] -> [[a]]
chunkify _ [] = []
chunkify l xs =
    let
        chunk = take l xs
        newList = drop l xs
    in
        chunk : chunkify l newList

main = do
    input <- getInput
    let parsedInput = map parseInput input
    let indexedMap = to2dIndexedMap parsedInput
    let sumOfFlashes = getSumOfFlashes 100 indexedMap
    print sumOfFlashes