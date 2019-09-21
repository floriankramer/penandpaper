{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Webserver 
    ( start 
    , handleRequest
    ) where


import qualified Network.Wai as W (responseLBS
                                  , responseFile
                                  , Application
                                  , pathInfo
                                  , Response)
import qualified Network.Wai.Handler.Warp as W (run)
import qualified Network.HTTP.Types as H (status200, status404)
import qualified Network.HTTP.Types.Header as H (hContentType)
import qualified Data.List as List
import qualified Data.String as String 
import qualified Data.Text as Text
import qualified Data.ByteString.UTF8 as UTF8

start :: IO ()
start = W.run 8080 handleRequest

handleRequest :: W.Application
handleRequest req f =
  let 
    pathlist = W.pathInfo req
    path = List.intercalate "/" $ map Text.unpack pathlist
  in
    if | path == "index.html" -> f $ fileToResponse path
       | path == "main.js" -> f $ fileToResponse path
       | path == "main.css" -> f $ fileToResponse path
       | path == "util.js" -> f $ fileToResponse path
       | path == "" -> f $ fileToResponse "index.html"
       | otherwise -> do
         putStrLn $ "404 for path: " ++ path
         f $ W.responseLBS H.status404 [(H.hContentType, "text/html")] "Page not found."

 

fileToResponse :: String -> W.Response
fileToResponse path = W.responseFile H.status200 [(H.hContentType, UTF8.fromString $ pathToContentType path)] ("html/" ++ path) Nothing


pathToContentType :: String -> String
pathToContentType path 
  | ending == ".js" = "applications/javascript"
  | otherwise = "text/html"
  where ending = pathToEnding path

pathFindLastDot :: String -> Maybe Int
pathFindLastDot s = List.elemIndex '.' $ reverse s

pathToEnding :: String -> String
pathToEnding path =
  let
    posLastDot = pathFindLastDot path
  in
    case posLastDot of
      Just pos -> drop pos path 
      Nothing -> ""
  
