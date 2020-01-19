import Control.Concurrent

rows = 5
cols = 5

data Action = Left | Right | Up | Down | None
data Point = Point Int Int deriving (Show)
data Player = Player Point deriving (Show)

init_board = replicate rows ( replicate cols False )

init_player = Player (Point 0 (div cols 2))

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
        let updated_board = update_board board player
            new_player = init_player
        in (updated_board, new_player)
    | otherwise = (board, player)

player_collision :: [[Bool]] -> Player -> Bool
player_collision board player =
    (player_touched_bottom player board) || (player_touched_brick player board)

player_touched_bottom :: Player -> [[Bool]] -> Bool
player_touched_bottom (Player (Point row _)) board = (row+1) == (length board)

player_touched_brick :: Player -> [[Bool]] -> Bool
player_touched_brick (Player (Point row col)) board =
    let target_row = board!!(row+1)
    in target_row!!col

update_board :: [[Bool]] -> Player -> [[Bool]]
update_board board (Player (Point row col)) =
    let target_row = board!!row
        row_with_player = (take col target_row) ++ [True] ++ (drop (col+1) target_row)
        updated_row = if all (\pixel -> pixel) row_with_player
            then replicate rows False
            else row_with_player
    in (take row board) ++ [updated_row] ++ (drop (row+1) board)

render_board :: [[Bool]] -> Player -> [[Bool]]
render_board board player =
    [[set_pixel col coli rowi player | (col, coli) <- zip row [0..]] | (row, rowi) <- zip board [0..]]

set_pixel :: Bool -> Int -> Int -> Player -> Bool
set_pixel col coli rowi (Player (Point player_row player_col)) =
    if rowi == player_row && coli == player_col then True else col

parse_action :: String -> Action
parse_action "s" = Main.Left
parse_action "f" = Main.Right
parse_action "e" = Main.Up
parse_action "d" = Main.Down
parse_action str = None

move_player :: Player -> Action -> Player
move_player (Player (Point row col)) Main.Left = Player (Point row (col-1))
move_player (Player (Point row col)) Main.Right = Player (Point row (col+1))
move_player (Player (Point row col)) Main.Up = Player (Point (row-1) col)
move_player (Player (Point row col)) Main.Down = Player (Point (row+1) col)
move_player player None = player

display_board :: [[Bool]] -> String
display_board board = foldl (\acc row -> acc ++ "\n" ++ (show row)) "" board
