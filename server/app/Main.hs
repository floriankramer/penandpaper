module Main where

import qualified Webserver 
import qualified Websocket
import qualified System.Random as R
import Data.Char (chr)

import Control.Concurrent (forkIO) 


genKey :: R.StdGen -> (Int, Int) -> Int -> String 
genKey gen limits len =
  if len == 0 then
    ""
  else
    let
      (val, newgen) = R.randomR limits gen
    in
      (chr val) : (genKey newgen limits $ len - 1)

main :: IO ()
main = do
  putStrLn "Starting the server"
  gen <- R.newStdGen
  let key = genKey gen (65, 90) 32
  putStrLn $ "WebKey " ++ "EHYAOFURPECKFEBTRCOZQRSEHLQMHWWU"
  Websocket.start $ Webserver.handleRequest "EHYAOFURPECKFEBTRCOZQRSEHLQMHWWU"

     
   
       
