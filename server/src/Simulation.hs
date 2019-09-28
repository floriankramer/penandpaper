{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simulation (Model, initModel, update, initPacket, Response (..)) where

import qualified Data.Aeson.Parser as J
import qualified Data.Aeson.Types as J
import qualified Data.Aeson as J
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text.Lazy.Encoding as ECL
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.HashMap.Strict as M
import qualified Data.Array as A
import Data.List.Split (splitOn)
import qualified Data.List as List
import System.Random as R
import Text.Read (readMaybe)

-- constants

colors = A.listArray (0, 23)
                     [ (201, 42, 42)
                     , (166, 30, 77)
                     , (134, 46, 156)
                     , (95, 61, 196)
                     , (54, 79, 199)
                     , (24, 100, 171)
                     , (11, 114, 133)
                     , (8, 127, 91)
                     , (43, 138, 62)
                     , (92, 148, 13)
                     , (230, 119, 0)
                     , (217, 72, 15)
                     -- Light colors
                     , (255, 84, 84)
                     , (186, 90, 170)
                     , (154, 92, 186)
                     , (190, 122, 220)
                     , (138, 160, 220)
                     , (100, 190, 220)
                     , (42, 184, 183)
                     , (32, 200, 170)
                     , (110, 198, 120)
                     , (142, 220, 66)
                     , (255, 179, 50)
                     , (255, 190, 70)
                     ] 
numColors = 24

-- networking

data Response = Reply Text
              | Broadcast Text
              | Forward

data RawPacket = RawPacket { rpt :: String }

instance J.FromJSON RawPacket where
  parseJSON (J.Object v) = RawPacket <$> v J..: "type"
 
data Packet = CreateCreature { ccX :: Float, ccY :: Float }
            | MoveCreature { mcId :: Int, mcX :: Float, mcY :: Float }
            | DeleteCreature { dcId :: Int }
            | InitState { initTokens :: [Token]
                        , initNextId :: Int
                        , initNextColor :: Int }
            | Chat { chatSender :: String, chatMessage :: String }          

instance J.FromJSON Packet where
  parseJSON j = do
    o <- J.parseJSON j
    let t = M.lookup "type" (o :: J.Object)
    let mdata = M.lookup "data" (o :: J.Object)
    case t of
      Just raws -> do
        s :: String <- J.parseJSON raws
        case mdata of
          Just rawd -> do
            d :: J.Object <- J.parseJSON rawd
            if s == "CreateCreature" then
              CreateCreature
              <$> d J..: "x" 
              <*> d J..: "y" 
            else if s == "MoveCreature" then
              MoveCreature
              <$> d J..: "id" 
              <*> d J..: "x" 
              <*> d J..: "y" 
            else if s == "DeleteCreature" then
              DeleteCreature
              <$> d J..: "id" 
            else if s == "Chat" then
              Chat 
              <$> d J..: "sender" 
              <*> d J..: "message" 
            else
              fail ("Unknown type " ++ s) :: (J.Parser Packet)
          Nothing ->
            fail "The packet is missing the data attribute." :: (J.Parser Packet)
      Nothing ->
        fail "The packet is missing the type attribute." :: (J.Parser Packet)

instance J.ToJSON Packet where
  toJSON p =
    case p of
      CreateCreature x y ->
        J.object [ "type" J..= ("CreateCreature" :: Text), "data" J..= J.object [
                   "x" J..= x,
                   "y" J..= y
                 ]]
      MoveCreature id x y ->
        J.object [ "type" J..= ("MoveCreature" :: Text), "data" J..= J.object [
                   "id" J..= id,
                   "x" J..= x,
                   "y" J..= y
                 ]]
      DeleteCreature id ->
        J.object [ "type" J..= ("DeleteCreature" :: Text), "data" J..= J.object [
                   "id" J..= id
                 ]]
      InitState tokens ni nc ->
        J.object ["type" J..= ("Init" :: Text)
                 , "data" J..= J.object [ "tokens" J..= J.toJSON tokens
                 , "nextId" J..=  ni
                 , "nextColor" J..=  nc
                 ]]
      Chat sender message ->
        J.object ["type" J..= ("Chat" :: Text)
                 , "data" J..= J.object [
                   "sender" J..= sender
                 , "message" J..= message
                 ]]


initPacket :: Model -> Text
initPacket model =
  TL.toStrict $ ECL.decodeUtf8 $ J.encode $ 
    InitState { initTokens = tokens model
              , initNextId = nextId model
              , initNextColor = nextColor model
              }

toJsonText :: (J.ToJSON a) => a -> Text
toJsonText = TL.toStrict . ECL.decodeUtf8 . J.encode

-- the model
data Token = Token { tokenId :: Int
                   , tokenX :: Float
                   , tokenY :: Float
                   , tokenRadius :: Float
                   , tokenR :: Float
                   , tokenG :: Float
                   , tokenB :: Float
                   }

instance J.ToJSON Token where
  toJSON t =
    J.object [ "id" J..= tokenId t
             , "x" J..= tokenX t
             , "y" J..= tokenY t
             , "radius" J..= tokenRadius t
             , "r" J..= tokenR t
             , "g" J..= tokenG t
             , "b" J..= tokenB t
             ]


data Model = Model { tokens :: [Token], nextId :: Int, nextColor :: Int
                   , mRand :: R.StdGen}

initModel :: R.StdGen -> Model
initModel rand = Model { tokens = [], nextId = 0, nextColor = 0, mRand = rand}

update :: Text -> Model -> (Model, Response)
update input model =
  let
    p = (J.decode $ ECL.encodeUtf8 $ TL.fromStrict input) :: Maybe Packet
  in
  case p of
    Just v ->
      case v of
        CreateCreature _ _ ->
          (onCreateCreature v model, Forward)
        MoveCreature _ _ _ ->
          (onMoveCreature v model, Forward)
        DeleteCreature _ ->
          (onDeleteCreature v model, Forward)
        Chat _ _ ->
          onChat v model
    Nothing ->
     (model, Forward)

onCreateCreature :: Packet -> Model -> Model
onCreateCreature p model =
  let
    (r, g, b) = colors A.! (nextColor model) 
  in
    model {
       nextId = (nextId model) + 1
     , tokens = Token { tokenId = nextId model
                      , tokenX = ccX p
                      , tokenY = ccY p
                      , tokenRadius = 0.25
                      , tokenR = r
                      , tokenG = g
                      , tokenB = b
                      } : (tokens model)
    , nextColor = mod ((nextColor model) + 1) numColors
    }

onMoveCreature :: Packet -> Model -> Model
onMoveCreature p model =
  model { tokens =
            applyToCreature (\t -> t {tokenX = mcX p, tokenY = mcY p})
                            (mcId p)
                            (tokens model)
        }
onDeleteCreature :: Packet -> Model -> Model
onDeleteCreature p model =
  let 
    i = dcId p
  in
    model { tokens =
              filter (\t -> tokenId t /= i)
                     (tokens model)
          }
  
onChat :: Packet -> Model -> (Model, Response)
onChat p model =
  let
    msg = chatMessage p
    sender = chatSender p
  in
    if length msg > 0 && head msg == '/' then
      if List.isPrefixOf "/roll" msg then
        (model, Reply $ toJsonText $ Chat "Server" (cmdRoll (mRand model) sender msg))
      else
        (model, Reply $ toJsonText $ Chat "Server" ("Unknown command: " ++ msg))
    else
      (model, Forward)

rollList :: R.StdGen -> Int -> [Int] -> [Int]
rollList rand min []    = []
rollList rand min maxes =
  let
    (n, nrand) = R.randomR (min, head maxes) rand
  in
    n : (rollList nrand min $ tail maxes)


cmdRoll :: R.StdGen -> String -> String -> String
cmdRoll rand who t =
  let
    parts = tail $ splitOn " " t
    ints = map (\t -> readMaybe t :: Maybe Int) parts
    validM = filter (\t -> case t of
                             Just _ -> True
                             Nothing -> False)
                     ints
    valid = map (\t -> case t of 
                             Just i -> i
                             Nothing -> -1)
                validM
    rands = rollList rand 1 valid 
    srands = map (\t -> show t) rands
    snums = List.intercalate " " srands 
    sdice = List.intercalate " " (map (\t -> show t) valid)
  in
    who ++ " rolled " ++ snums ++ " with dice " ++ sdice
     

-- util functions
applyToCreature :: (Token -> Token) -> Int -> [Token] -> [Token]
applyToCreature f id l =
  map (\t -> if (tokenId t) == id then f t  else t) l
