module Main where

import Lib

import Data.Int
import Data.String.Utils (rstrip)
import Data.Matrix
import Network.Socket hiding (recv)
import qualified Network.Socket.ByteString.Lazy as LNBS
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Serialize as S

import Control.Concurrent
import Control.Monad (when)

main :: IO ()
main = setupSocket


msgSize :: Int64
msgSize = 1024


port :: PortNumber
port = 4242


-- max: (9,9)
boardSize :: (Int,Int)
boardSize = (6,7)


setupSocket :: IO ()
setupSocket = do
    s <- socket AF_INET Stream 0
    setSocketOption s ReuseAddr 1    -- make socket immediately reusable - eases debugging
    bind s (SockAddrInet port iNADDR_ANY)
    listen s 2
    putStrLn $ "Server started ..."
    acceptLoop s


acceptLoop :: Socket -> IO ()
acceptLoop sSock = do
    -- accept 2 connections
    (cSock0,cAddr0) <- accept sSock
    (cSock1,cAddr1) <- accept sSock
    -- start the game
    setupGame (cSock0,cAddr0) (cSock1,cAddr1)
    -- close sockets
    mapM_ close [cSock0,cSock1]
    return ()


setupGame :: (Socket, SockAddr) -> (Socket, SockAddr) -> IO ()
setupGame (cSock0,_) (cSock1,_) = do
    let bcast msg = mapM_ (flip sendMsg msg) [cSock0,cSock1]
    
    LNBS.sendAll cSock0 (LC.pack "What's your name?\n")
    LNBS.sendAll cSock1 (LC.pack "What's your name?\n")
    
    response0 <- fmap (rstrip.LC.unpack) $ LNBS.recv cSock0 msgSize
    response1 <- fmap (rstrip.LC.unpack) $ LNBS.recv cSock1 msgSize

    let user0 = User {uid = 0, name = response0}
    let user1 = User {uid = 1, name = response1}

    putStrLn $ "User " ++ (name user0) ++ " logged in"
    putStrLn $ "User " ++ (name user1) ++ " logged in"

    sendMsg cSock0 $ LC.unpack $ S.encodeLazy user0
    sendMsg cSock1 $ LC.unpack $ S.encodeLazy user1

    gameLoop (cSock0,cSock1) (user0,user1) user0 (makeBoard (fst boardSize) (snd boardSize)) bcast


gameLoop :: (Socket,Socket) -> (User,User) -> User -> Board -> (String -> IO ()) -> IO ()
gameLoop (s0,s1) (user0,user1) currUser board bcast = do
    -- bcast board, current user
    threadDelay 500000
    bcast $ LC.unpack $ S.encodeLazy $ toLists board
    putStrLn $ "Server broadcasted board:\n" ++ (show board)

    threadDelay 500000
    bcast $ LC.unpack $ S.encodeLazy currUser
    putStrLn $ "Server broadcasted userid: " ++ show currUser
 
    -- check gameState
    if (getGameState board /= GameOn) then do
        putStrLn "Game finished"
    else do
        -- wait for cmd
        cmd <- fmap (decodeLazy' :: LC.ByteString -> Cmd) $ LNBS.recv (if (uid currUser) == 0 then s0 else s1) msgSize
        let board' = execCmd cmd (if (uid currUser) == 0 then user0 else user1) board

        putStrLn "\n---\n"
        let nextUser = (if (uid currUser) == 0 then user1 else user0)
        -- abort if the the current user quit
        when (cmd /= Quit) $ gameLoop (s0,s1) (user0,user1) nextUser board' bcast
    

sendMsg :: Socket -> String -> IO ()
sendMsg cSock msg = LNBS.sendAll cSock (LC.pack $ msg ++ "\n")


execCmd :: Cmd -> User -> Board -> Board
execCmd (Put col) user board = move user board col
execCmd _ _ board = board


move :: User -> Board -> Column -> Board
move user board col = if isLegalMove col board then put user col board else board


decodeLazy' :: S.Serialize a => LC.ByteString -> a
decodeLazy' bs = case S.decodeLazy bs of
    Left err -> error $ "Deserialization Error: " ++ err
    Right x -> x
