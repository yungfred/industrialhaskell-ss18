module Main where

import Lib
import UI

import Data.Int

import Data.String.Utils (rstrip)
import Control.Monad (when, forever)
import Data.Matrix (fromLists)


import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy (recv, sendAll)
import qualified Data.Serialize as S
import qualified Data.ByteString.Lazy.Char8 as LC

import Brick.BChan
import Control.Concurrent (forkIO,threadDelay)
import qualified System.Exit as Ex
import qualified Control.Exception as Exc


msgSize :: Int64
msgSize = 1024

main :: IO ()
main = setupConn


setupConn :: IO ()
setupConn = do
    hostname <- getString "hostname"
    port <- getString "port"
    addr <- resolve hostname port
    putStrLn "Waiting for other user..."
    Exc.bracket (open addr) close talk
    return ()


resolve :: String -> String -> IO AddrInfo
resolve host port = do
    let hints = defaultHints { addrSocketType = Stream }
    addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
    return addr


open :: AddrInfo -> IO Socket
open addr = do
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock $ addrAddress addr
    return sock


talk :: Socket -> IO ()
talk sock = do
    user <- createUser sock
    putStrLn $ "My User:" ++ (show user)
    gameLoop sock user
    return ()



gameLoop :: Socket -> User -> IO ()
gameLoop sock user = do
    toGui <- newBChan 10
    fromGui <- newBChan 10
    _ <- forkIO $ gui toGui fromGui $ uid user
    forever $ do
        -- get board and current user
        board <- fmap (fromLists.decodeLazy' :: LC.ByteString -> Board) $ recv sock msgSize
        currUser <- fmap (decodeLazy' :: LC.ByteString -> User) $ recv sock msgSize

        -- write it to the GUI
        writeBChan toGui $ BrdMsg board
        writeBChan toGui $ UsrMsg currUser

        -- check gameState
        let gameState = getGameState board
        when (gameState /= GameOn) $ do
            writeBChan toGui $ OvrMsg gameState
            _ <- readBChan fromGui
            Ex.exitWith Ex.ExitSuccess

        -- send cmd to server, if own turn
        when (uid currUser == uid user) $ do
            writeBChan toGui TrnMsg
            cmd <- readBChan fromGui
            writeBChan toGui TrnMsg
            threadDelay 500000
            sendAll sock $ S.encodeLazy (cmd :: Cmd)


createUser :: Socket -> IO (User)
createUser sock = do
    msg <- fmap LC.unpack $ recv sock msgSize
    putStr msg
    username <- fmap rstrip getLine
    sendAll sock $ LC.pack username
    user <- fmap decodeLazy' $ recv sock msgSize
    return user


getString :: String -> IO String
getString str = do
    putStr $ "Enter " ++ str ++ ": "
    input <- fmap rstrip getLine
    return input


decodeLazy' :: S.Serialize a => LC.ByteString -> a
decodeLazy' bs = case S.decodeLazy bs of
    Left err -> error $ "Deserialization Error: " ++ err
    Right x -> x
