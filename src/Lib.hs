module Lib
    (
    Board,
    makeBoard,
    getGameState,
    isLegalMove,
    put,
    GameState (..),
    Cmd (..),
    User (..),
    Column,
    Cell (..),
    Matrix (..) 
    ) where


-- IMPORTS
-- -------
import Data.Matrix
import Data.Maybe
import Data.List hiding (transpose)
import Data.Universe.Helpers

import qualified Data.Serialize as S
import GHC.Generics

instance S.Serialize Cmd
instance S.Serialize Cell
instance S.Serialize GameState
instance S.Serialize User

-- PRECONDITIONS
-- -------------
-- * there are only two users: one with uid 0 (token X), the other with uid 0 (token O)


-- DATA TYPES
-- ----------
data Cmd = Put Int | Quit deriving (Eq, Show, Generic)

-- defines User data type
-- there are exactly two users: one with uid 0 (token X), the other with uid 1 (token O)
data User = User {
    uid :: Int,
    name :: String
} deriving (Show, Eq, Generic)

toToken :: User -> Token
toToken user =
  case uid user of
    0 -> X
    1 -> O
    n -> error $ show n ++ " not a valid uid"

-- defines the content of the cells of the board
-- there's either no token (N), a Player 0 token (X) or a Player 1 token (O)
data Cell = N | X | O deriving (Eq, Show, Read, Generic) 
type Token = Cell
tokens :: [Token]
tokens = [X, O]

-- the game board is represented as a matrix of cells
-- thus, indices start at 1 and m ! (i, j) is the cell in the i-th row and j-th column
type Board = Matrix Cell

-- indices and counts
type Column = Int
type Rows = Int
type Cols = Int

-- for answering the question whether anybody won yet
-- GameOn: No winner, board isn't full
-- WX / WO: X won / O won
-- Draw: No winner, board is full
data GameState = GameOn | Wuid0 | Wuid1 | Draw deriving (Eq, Show, Generic)


-- GAME BOARD FUNCTIONS
-- --------------------

-- internal; don't export
-- map winning token to its GameState
-- PRECONDITION: Token is X or O; calling with N will throw exception
toWin :: Token -> GameState
toWin token =
  case token of
    X -> Wuid0
    O -> Wuid1
    N -> error $ show N ++ " not a valid token"

-- creates a board with given collumn and row count
-- PRECONDITION: rows and collumns both assumed to be > 4
makeBoard :: Rows -> Cols -> Board
makeBoard rows cols = matrix rows cols $ \_ -> N

-- checks whether move is valid
-- move is valid iff given collumn is in bounds and collumn isn't already full (that is, has an empty cell left)
-- does NOT check whose turn it is to throw the token
isLegalMove :: Column -> Board -> Bool
isLegalMove column board = column > 0 && column <= ncols board && board ! (1, column) == N

-- drops given token into given column into given board
-- MOVE ASSUMED TO BE VALID
put :: User -> Column -> Board -> Board
put user column board = put' (toToken user) column 1 board where
  put' t c r b =
    if nrows b >= r + 1 && b ! (r + 1, c) == N then
      put' t c (r + 1) b
    else
      setElem t (r, c) b

-- looks nice if printed, e.g with putStrLn
-- showBoard :: Board -> String
-- showBoard = prettyMatrix

-- checks game state
-- 4 equal tokens in a row, column or diagonal is considered a 'streak'
getGameState :: Board -> GameState
getGameState board =
  let winner = checkStreaks board in
    if isJust winner then
      fromJust winner
    else
      if isFull then
        Draw
      else
        GameOn
      where
        isFull = not $ elem N $ toList board

-- helper function for getGameState; don't export
-- for a given board, returns the Just <GameState for winner> or Nothing if there's no winner
-- the board is assumed to be valid and from an actual game, so there can only be one winner
checkStreaks :: Board -> Maybe GameState
checkStreaks board =
  let checks = dropWhile ((==) Nothing) [checkForToken token board | token <- tokens] in
    if length checks > 0 then head checks else Nothing
  where
    checkForToken :: Token -> Board -> Maybe GameState
    checkForToken t b =
      let
        streak = replicate 4 t
        rows = toLists b
        cols = toLists $ transpose b
        dias = concat [filter (\l -> length l >= 4) $ diagonals lists | lists <- [rows, reverse cols]]
      in
        if
          or [or $ map (streak `isInfixOf`) lists | lists <- [rows, cols, dias]]
        then
          Just $ toWin t
        else
          Nothing
        
