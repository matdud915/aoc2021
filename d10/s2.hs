{-# LANGUAGE OverloadedStrings #-}

import Data.Functor ((<&>))
import Data.List (sort)

getInput :: IO [String]
getInput = readFile "input.txt" <&> lines

openingBrackets :: String
openingBrackets = "([{<"

closingBrackets :: String
closingBrackets = ")]}>"

areBracketsMatching :: Char -> Char -> Bool
areBracketsMatching '[' ']' = True
areBracketsMatching '(' ')' = True
areBracketsMatching '{' '}' = True
areBracketsMatching '<' '>' = True
areBracketsMatching _ _ = False

removeNeighbouringBrackets :: String -> String
removeNeighbouringBrackets [] = []
removeNeighbouringBrackets [x] = [x]
removeNeighbouringBrackets (x:y:rest)
    | areBracketsMatching x y = removeNeighbouringBrackets rest
    | otherwise = x:removeNeighbouringBrackets (y:rest)

removeMatchingBrackets :: String -> String
removeMatchingBrackets ss =
    let
        rss = removeNeighbouringBrackets ss
    in
        if length rss == length ss then rss else removeMatchingBrackets rss

elemOf :: String -> String -> Bool
elemOf [] _ = False
elemOf (s:ss) pred = s `elem` pred || elemOf ss pred

getFirstClosingBracket :: String -> String
getFirstClosingBracket [] = []
getFirstClosingBracket (s:ss)
    | s `elem` closingBrackets = [s]
    | otherwise = getFirstClosingBracket ss

isIncomplete :: String -> Bool
isIncomplete [] = True
isIncomplete (s:ss) = s `elem` openingBrackets && isIncomplete ss

calcPoints :: String -> Int
calcPoints [] = 0
calcPoints ss = helper 0 ss
    where
        helper :: Int -> String -> Int
        helper sum [] = sum
        helper sum ('(':ss) = helper (sum * 5 + 1) ss
        helper sum ('[':ss) = helper (sum * 5 + 2) ss
        helper sum ('{':ss) = helper (sum * 5 + 3) ss
        helper sum ('<':ss) = helper (sum * 5 + 4) ss

main = do
  input <- getInput
  let withoutMatchingBrackets = map removeMatchingBrackets input
  let allIncomplete = filter isIncomplete withoutMatchingBrackets
  let scores = sort $ map (calcPoints . reverse) $ allIncomplete
  let middleIndex = floor $ fromIntegral (length scores) / 2
  print $ scores !! middleIndex
  