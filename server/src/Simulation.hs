{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simulation (Model, initModel, update, initPacket) where

import qualified Data.Aeson.Parser as J
import qualified Data.Aeson.Types as J
import qualified Data.Aeson as J
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text.Lazy.Encoding as ECL
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.HashMap.Strict as M
import qualified Data.Array as A

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

data RawPacket = RawPacket { rpt :: String }

instance J.FromJSON RawPacket where
  parseJSON (J.Object v) = RawPacket <$> v J..: "type"
 
data Packet = CreateCreature { ccX :: Float, ccY :: Float }
            | MoveCreature { mcId :: Int, mcX :: Float, mcY :: Float }
            | DeleteCreature { dcId :: Int }
            | InitState { initTokens :: [Token]
                        , initNextId :: Int
                        , initNextColor :: Int }

packetParser :: J.Value -> String -> J.Parser Packet
packetParser (J.Object v) t =
  CreateCreature <$> v J..: "x" <*> v J..: "y"

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


initPacket :: Model -> Text
initPacket model =
  TL.toStrict $ ECL.decodeUtf8 $ J.encode $ 
    InitState { initTokens = tokens model
              , initNextId = nextId model
              , initNextColor = nextColor model
              }

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


data Model = Model { tokens :: [Token], nextId :: Int, nextColor :: Int }

initModel :: Model
initModel = Model { tokens = [], nextId = 0, nextColor = 0 }

update :: Text -> Model -> Model
update input model =
  let
    p = (J.decode $ ECL.encodeUtf8 $ TL.fromStrict input) :: Maybe Packet
  in
  case p of
    Just v ->
      case v of
        CreateCreature _ _ ->
          onCreateCreature v model
        MoveCreature _ _ _ ->
          onMoveCreature v model
        DeleteCreature _ ->
          onDeleteCreature v model
    Nothing ->
      model

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
  
 

-- util functions
applyToCreature :: (Token -> Token) -> Int -> [Token] -> [Token]
applyToCreature f id l =
  map (\t -> if (tokenId t) == id then f t  else t) l
