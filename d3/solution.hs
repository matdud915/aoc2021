import Data.List (transpose)

getInput :: IO [String]
getInput = do
  content <- readFile "input.txt"
  return $ lines content

countBits :: String -> (Int, Int)
countBits = foldl f (0, 0)
  where
    f (ones, zeros) x
      | x == '1' = (ones + 1, zeros)
      | x == '0' = (ones, zeros + 1)

bitsToNumber :: String -> Int
bitsToNumber bits = btn $ map toIntArray bits
  where
    toIntArray x = read [x] :: Int
    btn (x:xs) =
      let power = round $ 2 ** fromIntegral (length xs)
       in (x * power) + btn xs
    btn _ = 0

getGammaBits :: [(Int, Int)] -> String
getGammaBits ((ones, zeros):xs) =
  if ones > zeros
    then '1' : fRec
    else '0' : fRec
  where
    fRec = getGammaBits xs
getGammaBits _ = []

getEpsilonBits :: [(Int, Int)] -> String
getEpsilonBits ((ones, zeros):xs) =
  if ones < zeros
    then '1' : fRec
    else '0' : fRec
  where
    fRec = getEpsilonBits xs
getEpsilonBits _ = []

countGeneratorRating :: [String] -> Int -> String
countGeneratorRating [x] _ = x
countGeneratorRating xs index =
  let (ones, zeros) = countBits $ transpose xs !! index
      predicate =
        if ones >= zeros
          then \x -> x !! index == '1'
          else \x -> x !! index == '0'
      filteredXs = filter predicate xs
   in countGeneratorRating filteredXs (index + 1)

countScrubberRating :: [String] -> Int -> String
countScrubberRating [x] _ = x
countScrubberRating xs index =
  let (ones, zeros) = countBits $ transpose xs !! index
      predicate =
        if zeros <= ones
          then \x -> x !! index == '0'
          else \x -> x !! index == '1'
      filteredXs = filter predicate xs
   in countScrubberRating filteredXs (index + 1)

main = do
  input <- getInput
  let countedBits = map countBits (transpose input)
  let gamma = getGammaBits countedBits
  let epsilon = getEpsilonBits countedBits
  print $ bitsToNumber gamma * bitsToNumber epsilon
  let generatorRating = countGeneratorRating input 0
  let scrubberRating = countScrubberRating input 0
  print $ bitsToNumber generatorRating * bitsToNumber scrubberRating

