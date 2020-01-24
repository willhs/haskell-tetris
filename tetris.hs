-- todo
-- rotate shapes
-- shape brick collision

import Control.Concurrent

rows = 5
cols = 5

data Action = Left | Right | Up | Down | None
data Point = Point Int Int deriving Show
data Player = Player Point [[Bool]] deriving Show

shape_l = [ [True, True, True, True] ]
shape_cap_l = [ [ True ],
                [ True, True, True, True] ]
shape_s = [ [False, False, True,  True],
            [True,  True,  False, False] ]
shape_box = [ [True, True],
              [True, True] ]
shape_base = [ [False, True, False],
               [True,  True, True] ]
shapes = [ shape_l, shape_cap_l, shape_s, shape_box, shape_base ]

init_board = replicate rows ( replicate cols False )
init_player = Player (Point 0 (div cols 2)) shape_l

main = do
    print "starting game"
    game_tick init_board init_player

game_tick board player = do
    action_str <- getLine
    let action = parse_action action_str
    let moved_player = move_player player action
    --putStrLn ("moved player " ++ (show moved_player))

    let (updated_board, updated_player) = game_update board moved_player

    let rendered_board = render_board updated_board updated_player
    putStrLn (display_board rendered_board)

    game_tick updated_board updated_player

game_update :: [[Bool]] -> Player -> ([[Bool]], Player)
game_update board player
    | player_collision board player =
        let player_board = render_board board player
            updated_board = if (is_any_row_complete player_board)
                            then scroll_board_down player_board
                            else player_board
            new_player = init_player
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
    any (\(row, rowi) -> is_brick_below_shape_row shape row rowi player_point board) (zip board [0..])

is_brick_below_shape_row :: [[Bool]] -> [Bool] -> Int -> Point -> [[Bool]] -> Bool
is_brick_below_shape_row shape row rowi (Point player_row player_col) board =
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
parse_action str = None

move_player :: Player -> Action -> Player
move_player (Player (Point row col) shape) Main.Left = Player (Point row (col-1)) shape
move_player (Player (Point row col) shape) Main.Right = Player (Point row (col+1)) shape
move_player (Player (Point row col) shape) Main.Up = Player (Point (row-1) col) shape
move_player (Player (Point row col) shape) Main.Down = Player (Point (row+1) col) shape
move_player player None = player

display_board :: [[Bool]] -> String
display_board board = foldl (\acc row -> acc ++ "\n" ++ (show row)) "" board
