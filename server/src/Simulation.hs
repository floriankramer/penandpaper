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
import qualified Debug.Trace as D

-- constants

colors = A.listArray (0, 10)
                     [ (240, 50, 50) -- red 
                     , (176, 30, 90) -- burgund
                     , (201, 20, 201) -- pink
                     , (120, 61, 196) -- purple
                     , (24, 100, 171) -- blue
                     , (24, 172, 171) -- turquoise 
                     , (8, 127, 91) -- blue-green
                     , (92, 148, 13) -- red-green
                     , (217, 72, 15)  -- orange
                     , (129, 96, 65)  -- brown 
                     ]

numColors = 10

-- networking

data Response = Reply Text
              | Broadcast Text
              | Forward

data RawPacket = RawPacket { rpt :: String }

instance J.FromJSON RawPacket where
  parseJSON (J.Object v) = RawPacket <$> v J..: "type"
 
data Packet = CreateToken { ccX :: Float, ccY :: Float }
            | MoveToken { mcId :: Int, mcX :: Float, mcY :: Float }
            | DeleteToken { dcId :: Int }
            | InitState { initTokens :: [Token]
                        , initDoodads :: [Doodad]
                        , initNextId :: Int
                        , initNextColor :: Int }
            | Chat { chatSender :: String, chatMessage :: String }
            | CreateDoodadLine { lSx :: Float
                               , lSy :: Float
                               , lEx :: Float
                               , lEy :: Float }
            | ClearDoodads
            | ClearTokens
            | TokenToggleFoe { ttfId  :: Int }
            | InitSession { isUid :: String}
            | Session { sId :: Int, sPlayerName :: String }

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
            if s == "CreateToken" then
              CreateToken
              <$> d J..: "x" 
              <*> d J..: "y" 
            else if s == "MoveToken" then
              MoveToken
              <$> d J..: "id" 
              <*> d J..: "x" 
              <*> d J..: "y" 
            else if s == "DeleteToken" then
              DeleteToken
              <$> d J..: "id" 
            else if s == "Chat" then
              Chat 
              <$> d J..: "sender" 
              <*> d J..: "message" 
            else if s == "CreateDoodadLine" then
              CreateDoodadLine 
              <$> d J..: "sx" 
              <*> d J..: "sy" 
              <*> d J..: "ex" 
              <*> d J..: "ey" 
            else if s == "ClearDoodads" then
              return ClearDoodads 
            else if s == "ClearTokens" then
              return ClearTokens 
            else if s == "TokenToggleFoe" then
              TokenToggleFoe 
              <$> d J..: "id" 
            else if s == "InitSession" then
              InitSession 
              <$> d J..: "uid" 
            else
              fail ("Unknown type " ++ s) :: (J.Parser Packet)
          Nothing ->
            fail "The packet is missing the data attribute." :: (J.Parser Packet)
      Nothing ->
        fail "The packet is missing the type attribute." :: (J.Parser Packet)

instance J.ToJSON Packet where
  toJSON p =
    case p of
      CreateToken x y ->
        J.object [ "type" J..= ("CreateToken" :: Text), "data" J..= J.object [
                   "x" J..= x,
                   "y" J..= y
                 ]]
      MoveToken id x y ->
        J.object [ "type" J..= ("MoveToken" :: Text), "data" J..= J.object [
                   "id" J..= id,
                   "x" J..= x,
                   "y" J..= y
                 ]]
      DeleteToken id ->
        J.object [ "type" J..= ("DeleteToken" :: Text), "data" J..= J.object [
                   "id" J..= id
                 ]]
      InitState tokens doodads ni nc ->
        J.object ["type" J..= ("Init" :: Text)
                 , "data" J..= J.object 
                                 [ "tokens" J..= J.toJSON tokens
                                 , "doodads" J..= J.toJSON doodads 
                                 , "nextId" J..=  ni
                                 , "nextColor" J..=  nc
                                 ]
                 ]
      Chat sender message ->
        J.object ["type" J..= ("Chat" :: Text)
                 , "data" J..= J.object [
                   "sender" J..= sender
                 , "message" J..= message
                 ]]
      CreateDoodadLine sx sy ex ey ->
        J.object ["type" J..= ("CreateDoodadLine" :: Text)
                 , "data" J..= J.object [
                   "sx" J..= sx 
                 , "sy" J..= sy
                 , "ex" J..= ex
                 , "ey" J..= ey
                 ]]
      ClearDoodads ->
        J.object ["type" J..= ("ClearDoodads" :: Text)
                 , "data" J..= J.object []]
      ClearTokens ->
        J.object ["type" J..= ("ClearTokens" :: Text)
                 , "data" J..= J.object []]
      TokenToggleFoe i ->
        J.object ["type" J..= ("CreateDoodadLine" :: Text)
                 , "data" J..= J.object [
                   "id" J..= i
                 ]]
      Session i name ->
        J.object ["type" J..= ("Session" :: Text)
                 , "data" J..= J.object [
                   "id" J..= i
                 , "name" J..= name
                 ]]


initPacket :: Model -> Text
initPacket model =
  TL.toStrict $ ECL.decodeUtf8 $ J.encode $ 
    InitState { initTokens = tokens model
              , initDoodads = doodads model
              , initNextId = nextTokenId model
              , initNextColor = nextColor model
              }

toJsonText :: (J.ToJSON a) => a -> Text
toJsonText = TL.toStrict . ECL.decodeUtf8 . J.encode

playerToSessionPacket :: Player -> Packet 
playerToSessionPacket p =
  Session { sId = playerId p, sPlayerName = playerName p}

-- the model
data Token = Token { tokenId :: Int
                   , tokenX :: Float
                   , tokenY :: Float
                   , tokenRadius :: Float
                   , tokenR :: Float
                   , tokenG :: Float
                   , tokenB :: Float
                   , tokenFoe :: Bool
                   }

data Doodad = DoodadLine { doodadId :: Int
                         , doodadSx :: Float
                         , doodadSy :: Float
                         , doodadEx :: Float
                         , doodadEy :: Float
                         }
                    
data Player = Player { playerId :: Int
                     , playerName :: String
                     }

data Model = Model { tokens :: [Token]
                   , doodads :: [Doodad]
                   , players :: M.HashMap String Player 
                   , nextTokenId :: Int
                   , nextPlayerId :: Int 
                   , nextColor :: Int
                   , mRand :: R.StdGen}

instance J.ToJSON Token where
  toJSON t =
    J.object [ "id" J..= tokenId t
             , "x" J..= tokenX t
             , "y" J..= tokenY t
             , "radius" J..= tokenRadius t
             , "r" J..= tokenR t
             , "g" J..= tokenG t
             , "b" J..= tokenB t
             , "foe" J..= tokenFoe t
             ]


instance J.ToJSON Doodad where
  toJSON d =
    case d of
      DoodadLine {} ->
        J.object [ "id" J..= doodadId d
                 , "type" J..= ("line" :: String)
                 , "sx" J..= doodadSx d
                 , "sy" J..= doodadSy d
                 , "ex" J..= doodadEx d
                 , "ey" J..= doodadEy d
                 ]

initModel :: R.StdGen -> Model
initModel rand = Model { tokens = []
                       , doodads = []
                       , players = M.empty
                       , nextTokenId = 0
                       , nextPlayerId = 0
                       , nextColor = 0
                       , mRand = rand}

newPlayer :: String -> Model -> (Model, Player)
newPlayer uid model =
  let
    player = Player (nextPlayerId model) ""
    nmodel = model { nextPlayerId = (nextPlayerId model) + 1
                   , players = M.insert uid player (players model) 
                   }
  in
    (nmodel, player)

update :: Text -> Model -> (Model, Response)
update input model =
  let
    p = (J.decode $ ECL.encodeUtf8 $ TL.fromStrict input) :: Maybe Packet
  in
  case p of
    Just v ->
      case v of
        CreateToken {} ->
          (onCreateToken v model, Forward)
        MoveToken {} ->
          (onMoveToken v model, Forward)
        DeleteToken {} ->
          (onDeleteToken v model, Forward)
        Chat {} ->
          onChat v model
        CreateDoodadLine {} ->
          (onDoodadLine v model, Forward)
        ClearDoodads ->
          (onClearDoodads v model, Forward)
        ClearTokens ->
          (onClearTokens v model, Forward)
        TokenToggleFoe {} ->
          (onTokenToggleFoe v model, Forward)
        InitSession {} ->
          onInitSession v model
    Nothing ->
     (model, Forward)

onCreateToken :: Packet -> Model -> Model
onCreateToken p model =
  let
    (r, g, b) = colors A.! (nextColor model) 
  in
    model {
       nextTokenId = (nextTokenId model) + 1
     , tokens = Token { tokenId = nextTokenId model
                      , tokenX = ccX p
                      , tokenY = ccY p
                      , tokenRadius = 0.25
                      , tokenR = r
                      , tokenG = g
                      , tokenB = b
                      , tokenFoe = False
                      } : (tokens model)
    , nextColor = mod ((nextColor model) + 1) numColors
    }

onMoveToken :: Packet -> Model -> Model
onMoveToken p model =
  model { tokens =
            applyToToken (\t -> t {tokenX = mcX p, tokenY = mcY p})
                            (mcId p)
                            (tokens model)
        }
onDeleteToken :: Packet -> Model -> Model
onDeleteToken p model =
  let 
    i = dcId p
  in
    model { tokens =
              filter (\t -> tokenId t /= i)
                     (tokens model)
          }

onTokenToggleFoe :: Packet -> Model -> Model
onTokenToggleFoe p model =
  let
    (r, g, b) = colors A.! (nextColor model) 
  in
    model { tokens =
              applyToToken (\t -> t {tokenFoe = not $ tokenFoe t})
                              (ttfId p)
                              (tokens model) 
          }
  
onChat :: Packet -> Model -> (Model, Response)
onChat p model =
  let
    msg = chatMessage p
    sender = chatSender p
  in
    if length msg > 0 && head msg == '/' then
      if List.isPrefixOf "/roll " msg then
        let 
          (resp, nrand) = (cmdRoll (mRand model) sender msg)
        in
          (model {mRand = nrand}, Broadcast $ toJsonText $ Chat "Server" resp)
      else if List.isPrefixOf "/rollp " msg then
        let 
          (resp, nrand) = (cmdRoll (mRand model) sender msg)
        in
          (model {mRand = nrand}, Reply $ toJsonText $ Chat "Server to you" resp)
      else
        (model, Reply $ toJsonText $ Chat "Server to you" ("Unknown command: " ++ msg))
    else
      (model, Forward)

onDoodadLine :: Packet -> Model -> Model
onDoodadLine p model =
  model {
     doodads = DoodadLine { doodadId = 0
                          , doodadSx = lSx p
                          , doodadSy = lSy p
                          , doodadEx = lEx p
                          , doodadEy = lEy p
                          } : (doodads model)
  }

onClearDoodads :: Packet -> Model -> Model
onClearDoodads p model =
  model { doodads = [] }

onClearTokens :: Packet -> Model -> Model
onClearTokens p model =
  model { tokens = [] }

onInitSession :: Packet -> Model -> (Model, Response)
onInitSession p model =
  let
    stored = M.lookup (isUid p) (players model)
  in
    case stored of
        Just player -> (model
                       , Reply $ toJsonText $ playerToSessionPacket player)
        Nothing ->
          let
            (nmodel, nplayer) = newPlayer (isUid p) model
          in
            ( nmodel
            , Reply $ toJsonText $ playerToSessionPacket nplayer)

rollList :: R.StdGen -> Int -> [Int] -> ([Int], R.StdGen)
rollList rand min []    = ([], rand)
rollList rand min maxes =
  let
    (n, nrand) = R.randomR (min, head maxes) rand
    (r, rand2) = (rollList nrand min $ tail maxes)
  in
    (n : r, rand2)

cmdRoll :: R.StdGen -> String -> String -> (String, R.StdGen)
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
    (rands, nrand) = rollList rand 1 valid 
    srands = map (\t -> show t) rands
    snums = List.intercalate " " srands 
    sdice = List.intercalate " " (map (\t -> show t) valid)
  in
    (who ++ " rolled " ++ snums ++ " with dice " ++ sdice, nrand)
     

-- util functions
applyToToken :: (Token -> Token) -> Int -> [Token] -> [Token]
applyToToken f id l =
  map (\t -> if (tokenId t) == id then f t  else t) l
