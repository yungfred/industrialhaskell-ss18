{-# LANGUAGE TemplateHaskell #-}

module UI (gui, Message (..), guiTest) where

import Lib
import Data.Matrix hiding (transpose)
import Brick
import Brick.BChan
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((&), (.~), (%~), (^.))
import qualified Graphics.Vty as V
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Char


-- TYPES AND THE APP
-- -----------------

-- No named resources.
type Name = ()

-- The custom event fed into the gui from the client thread.
data Message = BrdMsg Board | UsrMsg User | OvrMsg GameState | TrnMsg

-- The application state type.
data Game = Game { _board :: Board
                 , _ownId :: Int
                 , _outChan :: BChan Cmd
                 , _currUser :: User
                 , _opponentName :: String
                 , _turn :: Bool
                 , _gameState :: GameState
                 }

makeLenses ''Game

initGame :: BChan Cmd -> Int -> Game
initGame chan ownUid = Game
                { _board = makeBoard 6 7
                , _ownId = ownUid
                , _outChan = chan
                , _currUser = User { uid = -1, name = "" }
                , _opponentName = ""
                , _turn = False
                , _gameState = GameOn
                }

app :: App Game Message Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }


-- DRAWING AND ATTRMAP
-- -------------------

drawUI :: Game -> [Widget Name]
drawUI g = [C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g]

drawStats :: Game -> Widget Name
drawStats g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str " INFO ")
  $ hLimit (ncols b * 4)
  $ vLimit (nrows b * 2 + 1)
  $ vBox $ case g ^. gameState of
           GameOn -> gameOnStats g
           Wuid0 -> if g ^. ownId == 0 then gameWonStats g else gameLostStats g
           Wuid1 -> if g ^. ownId == 1 then gameWonStats g else gameLostStats g
           Draw -> gameDrawStats g
  where b = g ^. board

gameOnStats :: Game -> [Widget Name]
gameOnStats g =
  [ strWrap "To put a token, press the key corresponding to the column of your choice.\n\n"
  , let
      colourAttr =
        if g ^. turn then
          if g ^. ownId == 0 then xTxtAttr else oTxtAttr
        else
          if g ^. ownId == 0 then oTxtAttr else xTxtAttr
    in
      if g ^. turn then
        (withAttr colourAttr $ strWrap $ "It's your turn.")
      else
        (withAttr colourAttr $ strWrap $ "It's " ++ (g ^. opponentName) ++ "'s turn.")
  ]

gameWonStats :: Game -> [Widget Name]
gameWonStats g =
  [ C.center $ strWrap "Congratulations! :-)"
  , C.center $ withBorderStyle (BS.borderStyleFromChar '*') $ B.border $ withAttr (if g ^. ownId == 0 then xTxtAttr else oTxtAttr) $ strWrap "You have WON the game!"
  , C.center $ strWrap $ g ^. opponentName ++ " now sits in the corner and cries!"
  ]

gameLostStats :: Game -> [Widget Name]
gameLostStats g =
  [ C.center $ strWrap "Oh no! :-("
  , C.center $ withAttr (if g ^. ownId == 0 then oTxtAttr else xTxtAttr) $ strWrap "+You have LOST the game!+"
  , C.center $ strWrap "Now go sit in the corner and cry!"
  ]

gameDrawStats :: Game -> [Widget Name]
gameDrawStats g =
  [ C.center $ strWrap "What a match! :-O"
  , C.center $ withBorderStyle BS.unicodeRounded $ B.border $ strWrap "It's a DRAW!"
  , C.center $ strWrap $ g ^. opponentName ++ " can't wait to challenge you another time soon!"
  ]
                 
drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str " GAMEBOARD ")
  $ vBox
  $ rows ++ [index] 
  where
    rows = concat [replicate 2 $ hBox row | row <- map (map drawCell) (toLists (g ^. board))]
    index = str $ concat [" " ++ show i ++ "^ " | i <- [1..ncols (g ^. board)]]

drawCell :: Cell -> Widget Name
drawCell O = withAttr oAttr cw
drawCell X = withAttr xAttr cw
drawCell N = withAttr nAttr cw

cw :: Widget Name
cw = str "    "

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (oAttr, V.green `on` V.brightGreen)
  , (xAttr, V.blue `on` V.brightBlue)
  , (nAttr, V.black `on` V.black)
  , (oTxtAttr, V.green `on` V.black)
  , (xTxtAttr, V.blue `on` V.black)
  ]

oAttr, xAttr, nAttr, oTxtAttr, xTxtAttr :: AttrName
oAttr = attrName "oAttr"
xAttr = attrName "xAttr"
nAttr = attrName "nAttr"
oTxtAttr = attrName "oTxtAttr"
xTxtAttr = attrName "xTxtAttr"


-- EVENT HANDLING
-- --------------

handleEvent :: Game -> BrickEvent Name Message -> EventM Name (Next Game)
handleEvent g (AppEvent (BrdMsg b)) = continue $ g & board .~ b
handleEvent g (AppEvent (UsrMsg u)) = continue $ if g ^. ownId == uid u then g & currUser .~ u else g & currUser .~ u & opponentName .~ (name u)
handleEvent g (AppEvent (OvrMsg s)) = continue $ g & gameState .~ s
handleEvent g (AppEvent (TrnMsg  )) = continue $ g & turn %~ not
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = do { liftIO $ writeBChan (g ^. outChan) Quit ; halt g }
handleEvent g (VtyEvent (V.EvKey V.KEsc [])) = do { liftIO $ writeBChan (g ^. outChan) Quit ; halt g }
handleEvent g (VtyEvent (V.EvKey (V.KChar col) [])) = do { liftIO $ tryWrite col g ; continue $ g }
handleEvent g _ = continue g

tryWrite :: Char -> Game -> IO ()
tryWrite c g = if g ^. turn && isDigit c && isLegalMove (digitToInt c) (g ^. board) then writeBChan (g ^. outChan) (Put (digitToInt c)) else return ()


-- MAIN FUNCTION
-- -------------

gui :: BChan Message -> BChan Cmd -> Int -> IO ()
gui toGui fromGui ownUid = void $ customMain (V.mkVty V.defaultConfig) (Just toGui) app $ initGame fromGui ownUid

guiTest :: String -> GameState -> IO ()
guiTest oppName state = do { chan <- newBChan 10 ; simpleMain.head.drawUI $ initGame chan 0 & gameState .~ state & opponentName .~ oppName }
