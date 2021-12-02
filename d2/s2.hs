data Direction = Up | Down | Forward deriving(Show)
data Command = Command Direction Int deriving (Show)

data Submarine = Submarine { aim :: Int, depth :: Int, horizontal :: Int } deriving (Show)
defaultSubmarine = Submarine 0 0 0

parseStringToDirection :: String -> Direction
parseStringToDirection "forward" = Forward
parseStringToDirection "down" = Down
parseStringToDirection "up" = Up

parseWordsToCommand :: [String] -> Command
parseWordsToCommand [a, b] = Command (parseStringToDirection a) (read b :: Int)

parseLinesToCommands :: [String] -> [Command]
parseLinesToCommands = map (parseWordsToCommand . words)

calcSubmarine :: [Command] -> Submarine
calcSubmarine cs = calcSubmarineRec cs defaultSubmarine
  where calcSubmarineRec :: [Command] -> Submarine -> Submarine
        calcSubmarineRec ((Command Up x):tail) (Submarine a d h) = calcSubmarineRec tail (Submarine (a-x) d h)
        calcSubmarineRec ((Command Down x):tail) (Submarine a d h) = calcSubmarineRec tail (Submarine (a+x) d h)
        calcSubmarineRec ((Command Forward x):tail) (Submarine a d h) = calcSubmarineRec tail (Submarine a (d+a*x) (h+x))
        calcSubmarineRec _ s = s

getInputCommands :: IO [Command]
getInputCommands = do
  content <- readFile "input.txt"
  return $ parseLinesToCommands $ lines content

main = do
  commands <- getInputCommands
  let submarine = calcSubmarine commands
  print $ depth submarine * horizontal submarine