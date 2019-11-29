{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Websocket (start) where

import qualified Network.Wai as W 
import qualified Network.Wai.Handler.Warp as W
import qualified Network.Wai.Handler.WebSockets as WWS
import qualified Network.WebSockets as WS
import qualified Network.Wai.Handler.WarpTLS as W
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Control.Concurrent.MVar as MV
import qualified Control.Exception as EX
import qualified System.Random as R 

import qualified Simulation as S

tlsSettings :: W.TLSSettings
tlsSettings = W.tlsSettings "cert/certificate.pem" "cert/key.pem"

warpSettings :: W.Settings
warpSettings = W.setPort 8080 W.defaultSettings

data Client = Client {clientId :: Int, conn :: WS.Connection}

instance Eq Client where
  x == y = (clientId x) == (clientId y)

data State = State { clients :: [Client]
                   , nextId :: Int
                   , model :: S.Model
                   , sRand :: R.StdGen
                   }
initState :: R.StdGen -> State
initState rand = State { clients = []
                       , nextId = 0
                       , model = S.initModel rand
                       , sRand = rand}

-- Takes a webserver and adds a websocket server
start :: W.Application -> IO ()
start h = do 
    rand <- R.newStdGen
    serverState <- MV.newMVar $ initState rand
    W.runTLS tlsSettings warpSettings $
             WWS.websocketsOr WS.defaultConnectionOptions (handleConnection serverState) h
-- start h = W.run 8080$
--             WWS.websocketsOr WS.defaultConnectionOptions handleConnection h

addClient :: WS.Connection -> State -> State
addClient conn s = s
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
  

communicateForever :: MV.MVar State -> WS.Connection -> IO ()
communicateForever mstate connection = do
  -- handle communication with the client
  EX.catch (do
      msg <- WS.receiveData connection
      putStrLn $ Text.unpack msg
      -- Broadcast the message to all clients
      state <- MV.takeMVar mstate
      let (nmodel, action) = S.update msg (model state)
      let nstate = state { model = nmodel }
      case action of
        S.Forward ->
          mapM_ (\ c -> WS.sendTextData (conn c) msg) (clients nstate)
        S.Broadcast t ->
          mapM_ (\ c -> WS.sendTextData (conn c) t) (clients nstate)
        S.Reply t ->
          WS.sendTextData connection t
        S.Ignore ->
          return ()
      MV.putMVar mstate nstate 
      communicateForever mstate connection
    )
    $ \ (e :: WS.ConnectionException) -> putStrLn "Connection closed."


handleConnection :: MV.MVar State -> WS.PendingConnection -> IO ()
handleConnection mstate pending = do
  -- accept the connection
  putStrLn "Got a connection"
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  -- update the list of clients
  state <- MV.takeMVar mstate
  let clientId = nextId state in do
    WS.sendTextData conn $ S.initPacket (model state) 
    MV.putMVar mstate $ addClient conn state 
    -- TODO: send the current state to the client
    communicateForever mstate conn
    -- remove the client from the list of clients
    state <- MV.takeMVar mstate
    MV.putMVar mstate $ removeClient clientId state 
