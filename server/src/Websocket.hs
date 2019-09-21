{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Websocket (start) where

import qualified Network.Wai as W 
import qualified Network.Wai.Handler.Warp as W
import qualified Network.Wai.Handler.WebSockets as WWS
import qualified Network.WebSockets as WS
import qualified Network.Wai.Handler.WarpTLS as W
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Control.Concurrent.MVar as MV

tlsSettings :: W.TLSSettings
tlsSettings = W.tlsSettings "cert/certificate.pem" "cert/key.pem"

warpSettings :: W.Settings
warpSettings = W.setPort 8080 W.defaultSettings

data Client = Client {clientId :: Int, conn :: WS.Connection}

instance Eq Client where
  x == y = (clientId x) == (clientId y)

data State = State {clients :: [Client], nextId :: Int}
initState :: State
initState = State {clients = [], nextId = 0}

-- Takes a webserver and adds a websocket server
start :: W.Application -> IO ()
start h = do 
    serverState <- MV.newMVar initState
    W.runTLS tlsSettings warpSettings $
             WWS.websocketsOr WS.defaultConnectionOptions (handleConnection serverState) h
-- start h = W.run 8080$
--             WWS.websocketsOr WS.defaultConnectionOptions handleConnection h

addClient :: WS.Connection -> State -> State
addClient conn s = State
  { clients = Client {clientId = nextId s, conn = conn} : (clients s )
  , nextId = nextId s + 1
  }

deleteById :: Int -> [Client] -> [Client]
deleteById _ [] = []
deleteById i (a:as) 
  | clientId a == i = as
  | otherwise = a : deleteById i as

removeClient :: Int -> State -> State
removeClient i s =
  s { clients = deleteById i (clients s)}
  

handleConnection :: MV.MVar State -> WS.PendingConnection -> IO ()
handleConnection mstate pending = do
  -- accept the connection
  putStrLn "Got a connection"
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  -- update the list of clients
  state <- MV.takeMVar mstate
  let clientId = nextId state in do
    MV.putMVar mstate $ addClient conn state 
    -- handle communication with the client
    msg <- WS.receiveData conn
    putStrLn $ Text.unpack msg
    WS.sendTextData conn msg
    -- remove the client from the list of clients
    state <- MV.takeMVar mstate
    MV.putMVar mstate $ removeClient clientId state 
