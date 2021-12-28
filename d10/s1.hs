{-# LANGUAGE OverloadedStrings #-}

import Data.Functor ((<&>))

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

calcPoints :: String -> Int
calcPoints ")" = 3
calcPoints "]" = 57
calcPoints "}" = 1197
calcPoints ">" = 25137
calcPoints _ = 0

main = do
  input <- getInput
  let withoutMatchingBrackets = map removeMatchingBrackets input
  print $ sum $ map (calcPoints . getFirstClosingBracket) $ filter (`elemOf` closingBrackets) withoutMatchingBrackets
