import Control.Concurrent

rows = 4
cols = 4

data Action = Left | Right | Up | Down | None
data Point = Point Int Int
data Player = Player Point

init_board = replicate rows ( replicate cols False )

init_player = Player (Point (div rows 2) (div cols 2))

main = do
    print "starting game"
    game_tick init_board init_player

game_tick board player = do
    --threadDelay (frame_rate / 60)
    action_str <- getLine
    let action = parse_action action_str
    let updated_player = move_player player action
    let new_board = game_move board updated_player action
    putStrLn (display new_board)
    game_tick board updated_player

game_move :: [[Bool]] -> Player -> Action -> [[Bool]]
game_move board player action =
    [[set_pixel x xi yi player | (x, xi) <- zip y [0..]] | (y, yi) <- zip board [0..]]
    --in [if yi == playery then insert row else x | (row, yi) <- zip board [0..]]


set_pixel :: Bool -> Int -> Int -> Player -> Bool
set_pixel x xi yi (Player (Point playerx playery)) = if xi == playerx && yi == playery then True else x

parse_action :: String -> Action
parse_action "s" = Main.Left
parse_action "f" = Main.Right
parse_action "e" = Main.Up
parse_action "d" = Main.Down
parse_action str = None

move_player :: Player -> Action -> Player
move_player (Player (Point x y)) Main.Left = Player (Point (x-1) y)
move_player (Player (Point x y)) Main.Right = Player (Point (x+1) y)
move_player (Player (Point x y)) Main.Up = Player (Point x (y-1))
move_player (Player (Point x y)) Main.Down = Player (Point x (y+1))
move_player player None = player

display :: [[Bool]] -> String
display board = foldl (\acc x -> acc ++ "\n" ++ (show x)) "" board
