data Direction
  = Up
  | Down
  | Forward
  deriving (Show)

data Command =
  Command Direction Int
  deriving (Show)

parseStringToDirection :: String -> Direction
parseStringToDirection "forward" = Forward
parseStringToDirection "down" = Down
parseStringToDirection "up" = Up

parseWordsToCommand :: [String] -> Command
parseWordsToCommand [a, b] = Command (parseStringToDirection a) (read b :: Int)

parseLinesToCommands :: [String] -> [Command]
parseLinesToCommands = map (parseWordsToCommand . words)

calcHorizontalPosition :: [Command] -> Int
calcHorizontalPosition ((Command Forward x):tail) =
  x + calcHorizontalPosition tail
calcHorizontalPosition (_:tail) = calcHorizontalPosition tail
calcHorizontalPosition [] = 0

calcVerticalPosition :: [Command] -> Int
calcVerticalPosition ((Command Down x):tail) = x + calcVerticalPosition tail
calcVerticalPosition ((Command Up x):tail) = -x + calcVerticalPosition tail
calcVerticalPosition (_:tail) = calcVerticalPosition tail
calcVerticalPosition [] = 0

getInputCommands :: IO [Command]
getInputCommands = do
  content <- readFile "input.txt"
  return $ parseLinesToCommands $ lines content

main = do
  commands <- getInputCommands
  let vertical = calcVerticalPosition commands
  let horizontal = calcHorizontalPosition commands
  print (vertical * horizontal)
