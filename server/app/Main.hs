module Main where

import qualified Webserver 
import qualified Websocket

import Control.Concurrent (forkIO) 

main :: IO ()
main = do
  putStrLn "Starting the server"
  Websocket.start Webserver.handleRequest

     
   
       
