module Lib
    ( playGame
    , checkForVictory
    ) where

import System.IO
import Control.Exception

type Board = [[Int]]
type Turn = Int
type Line = [Int]
type Element = Int

main = playGame

playGame :: IO ()
playGame = do
  hSetBuffering stdout NoBuffering
  sizestr <- parseBoardSize False
  let size1 = (read sizestr :: Int) - 1
  let board = createNewBoard size1
  playRound board 0


parseBoardSize :: Bool -> IO String
parseBoardSize repeating = do
  if repeating == False
    then putStr "Please choose board size (range: [5,80], recommended: 15): "
    else putStr "Parsing error! Please try again: "
  input <- getLine
  result <- try (evaluate (read input :: Int)) :: IO (Either SomeException Int)
  case result of
    Left ex  -> parseBoardSize True
    Right val -> if checkIfBoardSizeCorrect val
                   then return input
                   else parseBoardSize True


checkIfBoardSizeCorrect :: Int -> Bool
checkIfBoardSizeCorrect parsedSize = (parsedSize >= 5 && parsedSize <= 80)


createNewBoard :: Int -> Board
createNewBoard size = [board'' | y <- [0..size]]
  where board'' = [-50 + (0*x) | x <- [0..size]]


playRound :: Board -> Turn -> IO ()
playRound board turn = do
  board2 <- makeMove board turn False
  printBoard board2
  putStrLn "------------------------"
  if (checkForVictory board2 turn) == True
    then putStrLn ("\n\nPLAYER " ++ show(turn) ++ " WINS!!!!!\n\n")
    else playRound board2 ((turn+1) `mod` 2)


checkForVictory :: Board -> Turn -> Bool
checkForVictory board turn =
  cfvHorizontal board turn ||
  cfvVertical board turn ||
  cfvSlash board turn ||
  cfvBackSlash board turn


cfvHorizontal :: Board -> Turn -> Bool
cfvHorizontal board turn = or [(cfvHorizontalForRow (board !! ctr) turn) | ctr <- [0..size]]
  where size = (length board) - 1


cfvHorizontalForRow :: Line -> Turn -> Bool
cfvHorizontalForRow row turn = or [(cfvHorizontalForRowFrom row turn ctr) | ctr <- [0..(size-4)]]
  where size = (length row) - 1


cfvHorizontalForRowFrom :: Line -> Turn -> Int -> Bool
cfvHorizontalForRowFrom row turn ctr = (sum ([(row !! (ctr+i)) | i <- [0..4]]) == 5 * turn)


cfvVertical :: Board -> Turn -> Bool
cfvVertical board turn = or [(cfvVerticalForColumn (createColumn board col) turn) | col <- [0..size]]
  where size = (length board) - 1


createColumn :: Board -> Int -> Line
createColumn board col = [((board !! row) !! col) | row <- [0..size]]
  where size = (length board) - 1


cfvVerticalForColumn :: Line -> Turn -> Bool
cfvVerticalForColumn column turn = or [(cfvVerticalForColumnFrom column turn ctr) | ctr <- [0..(size-4)]]
  where size = (length column) - 1


cfvVerticalForColumnFrom :: Line -> Turn -> Int -> Bool
cfvVerticalForColumnFrom column turn ctr = (sum ([(column !! (ctr+i)) | i <- [0..4]]) == 5 * turn)


cfvSlash :: Board -> Turn -> Bool
cfvSlash board turn = or [(cfvForSlash (createSlash board row col) turn) | row <- [0..(size-4)], col <- [0..(size-4)]]
  where size = (length board) - 1


createSlash :: Board -> Int -> Int -> Line
createSlash board row col = [((board !! (row+i)) !! (col+i)) | i <- [0..4]]


cfvForSlash :: Line -> Turn -> Bool
cfvForSlash slash turn = (sum slash == 5 * turn)


cfvBackSlash :: Board -> Turn -> Bool
cfvBackSlash board turn = or [(cfvForSlash (createBackSlash board row col) turn) | row <- [0..(size-4)], col <- [4..size]]
  where size = (length board) - 1


createBackSlash :: Board -> Int -> Int -> Line
createBackSlash board row col = [((board !! (row+i)) !! (col-i)) | i <- [0..4]]


makeMove :: Board -> Turn -> Bool -> IO Board
makeMove board turn repeating = do
  if repeating == False
    then putStrLn ("Player " ++ (show turn) ++ " makes a move")
    else putStrLn "\nInvalid move! Please try again.\n" 
  (dx, dy) <- getCoords board False
  if (validateMove board dx dy)
    then return (updateBoard board turn dx dy)
    else makeMove board turn True


validateMove :: Board -> Int -> Int -> Bool
validateMove board dx dy =
  if validateNotOutOfRange board dx dy == False
    then False
    else if validateCellNotEmpty board dx dy == False
      then False
      else True


validateNotOutOfRange :: Board -> Int -> Int -> Bool
validateNotOutOfRange board dx dy = dx >= 0 && dx <= maxSize && dy >= 0 && dy <= maxSize
  where maxSize = (length board) - 1


validateCellNotEmpty :: Board -> Int -> Int -> Bool
validateCellNotEmpty board dx dy = ((board !! dx) !! dy) < 0


getCoords :: Board -> Bool -> IO (Int, Int)
getCoords board repeating = do
  if repeating == True
    then putStrLn "Parsing error! Please try again!\n"
    else putStr ""
  putStr "x = "
  dx <- getLine
  putStr "y = "
  dy <- getLine
  validation <- validateInput dx dy
  if validation
    then return ((read dx :: Int), (read dy :: Int))
    else getCoords board True


validateInput :: String -> String -> IO Bool
validateInput dx dy = do
  correct1 <- validateValue dx
  correct2 <- validateValue dy
  return (correct1 && correct2)


validateValue :: String -> IO Bool
validateValue coord = do
  result <- try (evaluate (read coord :: Int)) :: IO (Either SomeException Int)
  case result of
    Left ex  -> return False
    Right val -> return True



updateBoard :: Board -> Turn -> Int -> Int -> Board
updateBoard board turn dx dy = [(updateRow row turn dx dy ctr) | (row,ctr) <- (zip board [0..size])]
  where size = length board


updateRow :: Line -> Turn -> Int -> Int -> Int -> Line
updateRow row turn dx dy ctr = 
  if dx == ctr
    then [(updateElement element turn dx dy ctr) | (element,ctr) <- (zip row [0..size])]
    else row
 where size = length row
    

updateElement :: Element -> Turn -> Int -> Int -> Int -> Int
updateElement element turn dx dy ctr = 
  if dy == ctr
    then turn
    else element


printBoard :: Board -> IO ()
printBoard board = printAllRows board 0


printAllRows :: Board -> Int -> IO ()
printAllRows board rowNo = do
  printRow (board !! rowNo)
  if (rowNo < maxRow)
    then printAllRows board (rowNo + 1)
    else putStr "\n"
 where maxRow = (length board) - 1


printRow :: Line -> IO()
printRow row = printAllElements row 0


printAllElements :: Line -> Int -> IO ()
printAllElements row elemNo = do
  printElem (row !! elemNo)
  if (elemNo < maxElem)
    then printAllElements row (elemNo + 1)
    else putStr "\n"
 where maxElem = (length row) - 1


printElem :: Element -> IO ()
printElem elem =
  if (elem < 0) || (elem > 1)
    then putStr " ,"
    else if elem == 1
      then putStr "x,"
      else putStr "o,"