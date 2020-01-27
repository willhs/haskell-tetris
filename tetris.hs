-- todo:
-- shapes constantly fall

import Control.Concurrent
import System.Random

rows = 8
cols = 8

data Action = Left | Right | Up | Down | Rotate | None deriving Eq
data Point = Point Int Int deriving Show
data Player = Player Point [[Bool]] deriving Show

shapeL = [ [True, True, True, True] ]
shapeCapL = [ [ True, False, False],
                [ True, True, True, True] ]
shapeS = [ [False, True, True ],
            [True,  True,  False ] ]
shapeBox = [ [True, True],
              [True, True] ]
shapeBase = [ [False, True, False],
               [True,  True, True] ]
shapes = [ shapeL, shapeCapL, shapeS, shapeBox, shapeBase ]

initBoard = replicate rows ( replicate cols False )
initPlayer randomGen =
    let shape = shapes!!(fst $ randomR (0, length shapes - 1) randomGen)
    in Player (Point 0 0) shape

main = do
    print "starting game"
    randomGen <- getStdGen
    let player = initPlayer randomGen
    gameTick initBoard player

gameTick board player = do

    let renderedBoard = renderBoard board player
    putStrLn (displayBoard renderedBoard)

    actionStr <- getLine
    let action = parseAction actionStr
    let movedPlayer = movePlayer player action board

    randomGen <- newStdGen
    let (updatedBoard, updatedPlayer) = gameUpdate board movedPlayer randomGen

    if isPlayerOnBrick board updatedPlayer then do
        putStrLn "game over man"
    else do
        gameTick updatedBoard updatedPlayer

gameUpdate :: [[Bool]] -> Player -> StdGen -> ([[Bool]], Player)
gameUpdate board player randomGen
    | playerCollision board player =
        let playerBoard = renderBoard board player
            updatedBoard = if (isAnyRowComplete playerBoard)
                            then scrollBoardDown playerBoard
                            else playerBoard
            newPlayer = initPlayer randomGen
        in (updatedBoard, newPlayer)
    | otherwise = (board, player)

playerCollision :: [[Bool]] -> Player -> Bool
playerCollision board player =
    (playerTouchedBottom player board) || (playerLandedOnBrick player board)

playerTouchedBottom :: Player -> [[Bool]] -> Bool
playerTouchedBottom (Player (Point row _) shape) board =
    let shapeHeight = length shape
    in (row + shapeHeight) == (length board)

playerLandedOnBrick :: Player -> [[Bool]] -> Bool
playerLandedOnBrick (Player playerPoint shape) board =
    any (\(row, rowi) -> isBrickBelowShapeRow row rowi playerPoint board) (zip shape [0..])

isBrickBelowShapeRow :: [Bool] -> Int -> Point -> [[Bool]] -> Bool
isBrickBelowShapeRow row rowi (Point playerRow playerCol) board =
    any (\(pixel, coli) -> if pixel
                            then (board!!(playerRow+rowi+1))!!(playerCol+coli)
                            else False
        ) (zip row [0..])

renderBoard :: [[Bool]] -> Player -> [[Bool]]
renderBoard board player =
    [renderRow row rowi player | (row, rowi) <- zip board [0..]]

renderRow :: [Bool] -> Int -> Player -> [Bool]
renderRow row rowi player =
    [renderPixel col coli rowi player | (col, coli) <- zip row [0..]]

renderPixel :: Bool -> Int -> Int -> Player -> Bool
renderPixel col coli rowi (Player (Point playerRow playerCol) shape)
    | inShapeBounds = col || ((shape!!(rowi - playerRow))!!(coli - playerCol))
    | otherwise = col
    where inShapeBounds = rowi >= playerRow && rowi < playerRow + (length shape)
                         && coli >= playerCol && coli < playerCol + (length (shape!!0))

isAnyRowComplete :: [[Bool]] -> Bool
isAnyRowComplete board = any (all (==True)) board

scrollBoardDown :: [[Bool]] -> [[Bool]]
scrollBoardDown board =
    map (\(row, i) -> if i == 0
                    then (replicate cols False)
                    else board!!(i-1))
                    (zip board [0..])

parseAction :: String -> Action
parseAction "s" = Main.Left
parseAction "f" = Main.Right
parseAction "e" = Main.Up
parseAction "d" = Main.Down
parseAction "r" = Main.Rotate
parseAction str = None

movePlayer :: Player -> Action -> [[Bool]] -> Player
movePlayer player action board
    | action == Main.Left || action == Main.Right =
        let movedPlayer = actionMap player action
            illegalMove = isPlayerOutOfBounds movedPlayer board || isPlayerOnBrick board movedPlayer
        in if illegalMove then player else movedPlayer
    | otherwise = actionMap player action

actionMap :: Player -> Action -> Player

actionMap (Player (Point row col) shape) Main.Left = Player (Point row (col-1)) shape
actionMap (Player (Point row col) shape) Main.Right = Player (Point row (col+1)) shape
actionMap (Player (Point row col) shape) Main.Up = Player (Point (row-1) col) shape
actionMap (Player (Point row col) shape) Main.Down = Player (Point (row+1) col) shape
actionMap (Player point shape) Main.Rotate = Player point (rotateShape shape)
actionMap player None = player

displayBoard :: [[Bool]] -> String
displayBoard board = foldl (\acc row -> acc ++ "\n" ++ (drawRow row)) "" board

drawRow :: [Bool] -> String
drawRow row = foldl (\row pixel -> row ++ (if pixel == True then "O" else ".")) "" row

isPlayerOnBrick :: [[Bool]] -> Player -> Bool
isPlayerOnBrick board (Player playerPoint shape) =
    any (\(row, rowi) -> isPlayerOnBrickRow row rowi playerPoint board) (zip shape [0..])

isPlayerOnBrickRow :: [Bool] -> Int -> Point -> [[Bool]] -> Bool
isPlayerOnBrickRow row rowi (Point playerRow playerCol) board =
    any (\(pixel, coli) -> if pixel
                            then (board!!(playerRow+rowi))!!(playerCol+coli)
                            else False
        ) (zip row [0..])

isPlayerOutOfBounds :: Player -> [[Bool]] -> Bool
isPlayerOutOfBounds (Player (Point row col) shape) board =
    col + length (shape!!0) > cols || col < 0

rotateShape :: [[Bool]] -> [[Bool]]
rotateShape shape =
    let shapeRows = length shape
        shapeCols = length $ shape!!0
    in [ [ shape!!(shapeRows-1-r)!!c | r <- [0..shapeRows-1]] | c <- [0..shapeCols-1] ]
