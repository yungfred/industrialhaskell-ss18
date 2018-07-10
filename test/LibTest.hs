import Lib

userX :: User
userX = User {uid = 0, name = "Herbert"}
userO :: User
userO = User {uid = 1, name = "Franz"}

boards :: [Board]
boards = [emptyBoard, board1, board2, board3, fullBoard, drawBoard]

emptyBoard :: Board
emptyBoard = makeBoard 6 7

board1 :: Board
board1 = put userX 5 $ put userO 4 $ put userX 5 emptyBoard

board2 :: Board
board2 = put userO 2 $ put userX 5 $ put userO 3 board1

board3 :: Board
board3 = put userX 5 $ board2

-- board full of O
fullBoard :: Board
fullBoard = foldl (flip ($)) emptyBoard [put userO col | col <- concat [replicate 6 i | i <- [1..7]]]

-- full board without winner: Draw
drawBoard :: Board
drawBoard =
  let
    toUser = \n -> if odd n then userX else userO
    turns = concat $ [replicate 2 (put (toUser $ i + j) i) | i <- [1..7], j <- [0..2]]
  in
    foldl (flip ($)) emptyBoard $ turns
