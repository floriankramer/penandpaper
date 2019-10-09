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
                                  , queryString
                                  , Response)
import qualified Network.Wai.Handler.Warp as W (run)
import qualified Network.HTTP.Types as H (status200, status404)
import qualified Network.HTTP.Types.Header as H (hContentType)
import qualified Data.List as List
import qualified Data.String as String 
import qualified Data.Text as Text
import qualified Data.ByteString.UTF8 as UTF8
import  Data.ByteString (ByteString)
import Debug.Trace (traceId, traceShow)

start :: String -> IO ()
start key = W.run 8080 $ handleRequest key

isKeyQuery :: String -> (ByteString, Maybe ByteString) -> Bool
isKeyQuery key (a, b) =
  if a == "key" then
    case b of
      Just v -> v == UTF8.fromString key 
      Nothing -> False
  else False


handleRequest :: String -> W.Application
handleRequest key req f =
  let 
    pathlist = W.pathInfo req
    path = List.intercalate "/" $ map Text.unpack pathlist
    qsa = filter (isKeyQuery key) (W.queryString req)
    hasKey = length qsa > 0
  in
    if | path == "index.html" && hasKey -> f $ fileToResponse path
       | path == "main.js" -> f $ fileToResponse path
       | path == "main.css" -> f $ fileToResponse path
       | path == "util.js" -> f $ fileToResponse path
       | path == "" && hasKey -> f $ fileToResponse "index.html"
       | head pathlist == "images" && length pathlist == 2 -> f $ fileToResponse path
       | otherwise -> do
         putStrLn $ "404 for path: " ++ path
         f $ W.responseLBS H.status404 [(H.hContentType, "text/html")] "Page not found."

 

fileToResponse :: String -> W.Response
fileToResponse path = W.responseFile H.status200 [(H.hContentType, UTF8.fromString $ pathToContentType path)] ("html/" ++ path) Nothing


pathToContentType :: String -> String
pathToContentType path 
  | ending == ".js" = "applications/javascript"
  | ending == ".css" = "applications/css"
  | ending == ".png" = "image/png"
  | ending == ".jpg" = "image/jpeg"
  | otherwise = "text/html"
  where ending = pathToEnding path

pathFindLastDot :: String -> Maybe Int
pathFindLastDot s = 
  let
    i = (List.elemIndex '.' $ reverse s)
  in 
    case i of 
      Just j -> Just $ (length s) - j - 1
      Nothing -> Nothing 


pathToEnding :: String -> String
pathToEnding path =
  let
    posLastDot = pathFindLastDot path
  in
    case posLastDot of
      Just pos -> drop pos path 
      Nothing -> ""
  
