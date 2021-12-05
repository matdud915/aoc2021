import Data.List (transpose, delete)

data BingoCell = BingoCell { number :: String, marked :: Bool } deriving (Show, Eq)
newtype BingoRow = BingoRow [BingoCell] deriving (Show, Eq)
newtype Board = Board [BingoRow] deriving (Show, Eq)

markNumber :: String -> Board -> Board
markNumber number (Board rows) = Board $ map mapRow rows
    where
      mapCell (BingoCell n m) = BingoCell n (m || n == number)
      mapRow (BingoRow row) = BingoRow $ map mapCell row

markBoards :: String -> [Board] -> [Board]
markBoards number = map (markNumber number)

checkIfBoardWins :: [Board] -> Bool
checkIfBoardWins = foldl winReducer False
  where 
    winReducer acc b = acc || checkBoard b
    checkBoard (Board rows) = (checkRawBoard $ toRawBoard rows) || (checkRawBoard $ transpose $ toRawBoard rows)
    toRawBoard = map (\(BingoRow cs) -> (map marked cs))
    checkRawBoard (r:rs) = (foldl boardReducer True r) || (checkRawBoard rs)
    checkRawBoard [] = False
    boardReducer acc r = acc && r

getWinBoard :: [Board] -> Board
getWinBoard (b:bs) = if checkIfBoardWins [b] == True then b else getWinBoard bs

getWinBoards :: [Board] -> [Board]
getWinBoards [] = []
getWinBoards (b:bs) = if checkIfBoardWins [b] == True then b : getWinBoards bs else getWinBoards bs

deleteList :: [Board] -> [Board] -> [Board]
deleteList [] bs = bs
deleteList (p:ps) bs = deleteList ps $ delete p bs

countUncrossedNumber :: [Board] -> Int
countUncrossedNumber = foldl boardReducer 0
  where
    boardReducer acc (Board b) = acc + foldl rowsReducer 0 b
    rowsReducer acc (BingoRow r) = acc + foldl cellReducer 0 r
    cellReducer acc (BingoCell n False) = acc + (read n :: Int)
    cellReducer acc (BingoCell n True) = acc + 0

splitBy :: Char -> String -> [String]
splitBy _ [] = []
splitBy by str =
  let
    (prefix, tailWithBy) = break (== by) str
    tail = drop 1 tailWithBy
  in
    if null prefix then [tail]
    else prefix : splitBy by tail

getInput :: IO [String]
getInput = do
  content <- readFile "input.txt"
  return $ lines content

getRow :: String -> BingoRow
getRow = BingoRow . map (`BingoCell` False) . words

getRows :: [String] -> [BingoRow]
getRows = map getRow

getBoard :: [String] -> Board
getBoard = Board . getRows 

getBoards :: [String] -> [Board]
getBoards [] = []
getBoards (a:b:c:d:e:rest) = getBoard [a,b,c,d,e] : getBoards (drop 1 rest)

getBingoNumbers :: String -> [String]
getBingoNumbers = splitBy ','

playBingo :: [String] -> [Board] -> (String, [Board])
playBingo (n:ns) boards = 
  let
    crossedBoards = markBoards n boards
    won = checkIfBoardWins crossedBoards
  in
    if won then (n, crossedBoards) else playBingo ns crossedBoards

playBingoTilTheEnd :: [String] -> [Board] -> (String, [Board])
playBingoTilTheEnd n boards = play n boards ("", [])
  where
    play [] boards currentWinBoards = currentWinBoards
    play (n:ns) boards currentWinBoards = 
      let
        crossedBoards = markBoards n boards
        won = checkIfBoardWins crossedBoards
        turnWinBoards = if won then getWinBoards crossedBoards else (snd currentWinBoards)
        newCrossedBoards = if won then deleteList turnWinBoards crossedBoards else crossedBoards
      in
        if won then play ns newCrossedBoards (n, turnWinBoards) else play ns newCrossedBoards currentWinBoards

main = do
  input <- getInput
  let bingoNumbers = getBingoNumbers $ head input
  let boards = getBoards (drop 2 input)
  let (wonNumber, endBoards) = (playBingo bingoNumbers boards)
  let winBoard = getWinBoard endBoards
  let uncrossedSum = countUncrossedNumber [winBoard]
  print $ (read wonNumber :: Int) * uncrossedSum
  let (lastWonNumber, lastWinBoard) = playBingoTilTheEnd bingoNumbers boards
  let lastUncrossedSum = countUncrossedNumber lastWinBoard
  print $ (read lastWonNumber :: Int) * lastUncrossedSum

