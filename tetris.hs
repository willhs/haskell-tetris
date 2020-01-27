-- todo:
-- shapes constantly fall

import Control.Concurrent
import System.Random

rows = 8
cols = 8

data Action = Left | Right | Up | Down | Rotate | None deriving Eq
data Point = Point Int Int deriving Show
data Player = Player Point [[Bool]] deriving Show

shape_l = [ [True, True, True] ]
shape_cap_l = [ [ True, False, False],
                [ True, True, True, True] ]
shape_s = [ [False, True, True ],
            [True,  True,  False ] ]
shape_box = [ [True, True],
              [True, True] ]
shape_base = [ [False, True, False],
               [True,  True, True] ]
shapes = [ shape_l, shape_cap_l, shape_s, shape_box, shape_base ]

init_board = replicate rows ( replicate cols False )
init_player random_gen =
    let shape = shapes!!(fst $ randomR (0, length shapes - 1) random_gen)
    in Player (Point 0 0) shape

main = do
    print "starting game"
    random_gen <- getStdGen
    let player = init_player random_gen
    game_tick init_board player

game_tick board player = do

    let rendered_board = render_board board player
    putStrLn (display_board rendered_board)

    action_str <- getLine
    let action = parse_action action_str
    let moved_player = move_player player action board

    random_gen <- newStdGen
    let (updated_board, updated_player) = game_update board moved_player random_gen

    if is_player_on_brick board updated_player then do
        putStrLn "game over man"
    else do
        game_tick updated_board updated_player

game_update :: [[Bool]] -> Player -> StdGen -> ([[Bool]], Player)
game_update board player random_gen
    | player_collision board player =
        let player_board = render_board board player
            updated_board = if (is_any_row_complete player_board)
                            then scroll_board_down player_board
                            else player_board
            new_player = init_player random_gen
        in (updated_board, new_player)
    | otherwise = (board, player)

player_collision :: [[Bool]] -> Player -> Bool
player_collision board player =
    (player_touched_bottom player board) || (player_landed_on_brick player board)

player_touched_bottom :: Player -> [[Bool]] -> Bool
player_touched_bottom (Player (Point row _) shape) board =
    let shape_height = length shape
    in (row + shape_height) == (length board)

player_landed_on_brick :: Player -> [[Bool]] -> Bool
player_landed_on_brick (Player player_point shape) board =
    any (\(row, rowi) -> is_brick_below_shape_row row rowi player_point board) (zip shape [0..])

is_brick_below_shape_row :: [Bool] -> Int -> Point -> [[Bool]] -> Bool
is_brick_below_shape_row row rowi (Point player_row player_col) board =
    any (\(pixel, coli) -> if pixel
                            then (board!!(player_row+rowi+1))!!(player_col+coli)
                            else False
        ) (zip row [0..])

render_board :: [[Bool]] -> Player -> [[Bool]]
render_board board player =
    [render_row row rowi player | (row, rowi) <- zip board [0..]]

render_row :: [Bool] -> Int -> Player -> [Bool]
render_row row rowi player =
    [render_pixel col coli rowi player | (col, coli) <- zip row [0..]]

render_pixel :: Bool -> Int -> Int -> Player -> Bool
render_pixel col coli rowi (Player (Point player_row player_col) shape)
    | in_shape_bounds = col || ((shape!!(rowi - player_row))!!(coli - player_col))
    | otherwise = col
    where in_shape_bounds = rowi >= player_row && rowi < player_row + (length shape)
                         && coli >= player_col && coli < player_col + (length (shape!!0))

is_any_row_complete :: [[Bool]] -> Bool
is_any_row_complete board = any (\row -> all (==True) row) board

scroll_board_down :: [[Bool]] -> [[Bool]]
scroll_board_down board =
    map (\(row, i) -> if i == 0
                    then (replicate cols False)
                    else board!!(i-1))
                    (zip board [0..])

parse_action :: String -> Action
parse_action "s" = Main.Left
parse_action "f" = Main.Right
parse_action "e" = Main.Up
parse_action "d" = Main.Down
parse_action "r" = Main.Rotate
parse_action str = None

move_player :: Player -> Action -> [[Bool]] -> Player
move_player player action board
    | action == Main.Left || action == Main.Right =
        let moved_player = action_map player action
            illegal_move = is_player_out_of_bounds moved_player board || is_player_on_brick board moved_player
        in if illegal_move then player else moved_player
    | otherwise = action_map player action

action_map :: Player -> Action -> Player

action_map (Player (Point row col) shape) Main.Left = Player (Point row (col-1)) shape
action_map (Player (Point row col) shape) Main.Right = Player (Point row (col+1)) shape
action_map (Player (Point row col) shape) Main.Up = Player (Point (row-1) col) shape
action_map (Player (Point row col) shape) Main.Down = Player (Point (row+1) col) shape
action_map (Player point shape) Main.Rotate = Player point (rotate_shape shape)
action_map player None = player

display_board :: [[Bool]] -> String
display_board board = foldl (\acc row -> acc ++ "\n" ++ (draw_row row)) "" board

draw_row :: [Bool] -> String
draw_row row = foldl (\row pixel -> row ++ (if pixel == True then "O" else ".")) "" row

is_player_on_brick :: [[Bool]] -> Player -> Bool
is_player_on_brick board (Player player_point shape) =
    any (\(row, rowi) -> is_player_on_brick_row row rowi player_point board) (zip shape [0..])

is_player_on_brick_row :: [Bool] -> Int -> Point -> [[Bool]] -> Bool
is_player_on_brick_row row rowi (Point player_row player_col) board =
    any (\(pixel, coli) -> if pixel
                            then (board!!(player_row+rowi))!!(player_col+coli)
                            else False
        ) (zip row [0..])

is_player_out_of_bounds :: Player -> [[Bool]] -> Bool
is_player_out_of_bounds (Player (Point row col) shape) board =
    col + length (shape!!0) > cols || col < 0

rotate_shape :: [[Bool]] -> [[Bool]]
rotate_shape shape =
    let shape_rows = length shape
        shape_cols = length $ shape!!0
    in [ [ shape!!(shape_rows-1-r)!!c | r <- [0..shape_rows-1]] | c <- [0..shape_cols-1] ]
