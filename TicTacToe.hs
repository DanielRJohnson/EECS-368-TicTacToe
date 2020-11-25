-- Daniel Johnson
-- EECS 368: WS5: write a basic program in Haskell
-- 11/24/2020

import Data.Char

type Pos = (Int, Int)

data Turn = X | O | Empty
    deriving (Show, Eq)

swapTurn :: Turn -> Turn
swapTurn X = O
swapTurn O = X

type Board = [[Turn]]

splat :: Board -> IO()
splat brd = do
    putStrLn "==================="
    putStrLn (show (brd !! 0))
    putStrLn (show (brd !! 1))
    putStrLn (show (brd !! 2))
    putStrLn "==================="

replace1d :: Int -> a -> [a] -> [a]
replace1d i item xs = take i xs ++ [item] ++ tail (drop i xs)

replace2d :: Int -> Int -> a -> [[a]] -> [[a]]
replace2d row col item xs = 
    case row of 
        0 -> [(replace1d col item (xs !! 0)) , (xs !! 1) , (xs !! 2)]
        1 -> [(xs !! 0) , (replace1d col item (xs !! 1)) , (xs !! 2)]
        2 -> [(xs !! 0) , (xs !! 1) , (replace1d col item (xs !! 2))]

data State = State Board Turn

data Outcome = XWin | OWin | Tie | None
    deriving (Show, Eq)

main = do
    putStrLn "=================================="
    putStrLn "Welcome to TicTacToe with Haskell"
    putStrLn "=================================="
    let brd = [[Empty, Empty, Empty],[Empty, Empty, Empty],[Empty, Empty, Empty]]
    let st = State brd O
    gameLoop st

gameLoop :: State -> IO()
gameLoop st@(State brd turn) = do
    let winner = checkWinner brd
    if winner == XWin
    then putStrLn "X Wins! Exiting..."
    else if winner == OWin
    then putStrLn "O Wins! Exiting..."
    else if winner == Tie
    then putStrLn "Tie Game! Exiting..."
    else do
        input <- getInput
        let col = fst input
        let row = snd input

        if brd !! row !! col == Empty
        then do
            let turn' = swapTurn turn
            if turn' == X
            then do
                let brd' = replace2d row col turn' brd 
                splat brd'
                let st' = State brd' turn'
                gameLoop st'
            else do
                let brd' = replace2d row col turn' brd
                splat brd'
                let st' = State brd' turn'
                gameLoop st'
        else do
            putStrLn "Spot is not Empty. Try Again: "
            gameLoop st


getInput :: IO Pos
getInput = do
    putStrLn "Input the column of your move (0-2): "
    inp <- getLine
    let col = read ([head inp])
    putStrLn "Input the row of your move (0-2): "
    inp2 <- getLine
    let row = read ([head inp2])

    if col >= 0 && col <= 2 && row >= 0 && row <= 2
    then return (col, row)
    else do
        putStrLn "The row and columns have to be an integer 0-2. Try Again:"
        getInput

{- TERRIBLE IMPLEMENTATION OF A WINNER FUNCTION. I'M SORRY -}
checkWinner :: Board -> Outcome
--Rows
checkWinner [[X,X,X],[_,_,_],[_,_,_]] = XWin
checkWinner [[_,_,_],[X,X,X],[_,_,_]] = XWin
checkWinner [[_,_,_],[_,_,_],[X,X,X]] = XWin
--Cols
checkWinner [[X,_,_],[X,_,_],[X,_,_]] = XWin
checkWinner [[_,X,_],[_,X,_],[_,X,_]] = XWin
checkWinner [[_,_,X],[_,_,X],[_,_,X]] = XWin
--Left Diagonal
checkWinner [[X,_,_],[_,X,_],[_,_,X]] = XWin
--Right Diagonal
checkWinner [[_,_,X],[_,X,_],[X,_,_]] = XWin

--Rows
checkWinner [[O,O,O],[_,_,_],[_,_,_]] = OWin
checkWinner [[_,_,_],[O,O,O],[_,_,_]] = OWin
checkWinner [[_,_,_],[_,_,_],[O,O,O]] = OWin
--Cols
checkWinner [[O,_,_],[O,_,_],[O,_,_]] = OWin
checkWinner [[_,O,_],[_,O,_],[_,O,_]] = OWin
checkWinner [[_,_,O],[_,_,O],[_,_,O]] = OWin
--Left Diagonal
checkWinner [[O,_,_],[_,O,_],[_,_,O]] = OWin
--Right Diagonal
checkWinner [[_,_,O],[_,O,_],[O,_,_]] = OWin
--Else
checkWinner brd = 
    if Empty `elem` (brd !! 0) || Empty `elem` (brd !! 1) || Empty `elem` (brd !! 2) 
    then None
    else Tie