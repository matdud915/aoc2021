parseLinesToNumbers :: [String] -> [Int]
parseLinesToNumbers = map (\x -> read x :: Int)

countLargerThanPrevious :: [Int] -> Int
countLargerThanPrevious (a:tail@(b:_)) =
  if b > a
    then 1 + r
    else r
  where
    r = countLargerThanPrevious tail
countLargerThanPrevious _ = 0

sumThreeConsecutiveElements :: [Int] -> [Int]
sumThreeConsecutiveElements (a:tail@(b:c:_)) =
  (a + b + c) : sumThreeConsecutiveElements tail
sumThreeConsecutiveElements [a, b] = (a + b) : sumThreeConsecutiveElements [b]
sumThreeConsecutiveElements a = a

getInputNumbers :: IO [Int]
getInputNumbers = do
  content <- readFile "input.txt"s
  return $ parseLinesToNumbers $ lines content

main = do
  numbers <- getInputNumbers
  print $ countLargerThanPrevious numbers
  print $ countLargerThanPrevious $ sumThreeConsecutiveElements numbers
