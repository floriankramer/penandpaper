port module Main exposing (..)

import Browser exposing (Document)
import Browser.Dom as Dom exposing (getViewport)
import Browser.Events
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick, onInput)
import Html.Attributes as A
import Canvas  as C
import Canvas.Settings as C
import Canvas.Settings.Advanced as C
import Canvas.Settings.Line as C
import Canvas.Settings.Text as C
import Color exposing (Color)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Wheel as Wheel
import List
import String
import Json.Decode as Json
import Json.Encode as JE
import Debug
import Round as R
import Keyboard.Event as Keyboard
import Keyboard.Key as Key
import Task
import Array

-- Setup
main =
  Browser.document { init = init, update = update, subscriptions = subscriptions, view = view }

-- Model

baseColors = Array.fromList
              [ Color.rgb255 201 42 42 
              , Color.rgb255 166 30 77 
              , Color.rgb255 134 46 156 
              , Color.rgb255 95 61 196 
              , Color.rgb255 54 79 199 
              , Color.rgb255 24 100 171 
              , Color.rgb255 11 114 133 
              , Color.rgb255 8 127 91 
              , Color.rgb255 43 138 62 
              , Color.rgb255 92 148 13 
              , Color.rgb255 230 119 0 
              , Color.rgb255 217 72 15 
              -- Light colors
              , Color.rgb255 255 84 84
              , Color.rgb255 186 90 170
              , Color.rgb255 154 92 186 
              , Color.rgb255 190 122 220 
              , Color.rgb255 138 160 220 
              , Color.rgb255 100 190 220 
              , Color.rgb255 42 184 183 
              , Color.rgb255 32 200 170
              , Color.rgb255 110 198 120 
              , Color.rgb255 142 220 66
              , Color.rgb255 255 179 50 
              , Color.rgb255 255 190 70 
              ]
numBaseColors : Int
numBaseColors = 24

gmId : Int
gmId = 0

type Action = None
            | DragToken { startx : Float, starty : Float, x : Float, y : Float
                        , ox : Float, oy : Float }
            | DragView {lastX : Float, lastY : Float}
            | ActionCreateLine { sx : Float
                               , sy : Float
                               , ex : Float
                               , ey : Float
                               }

type CreateMode = ModeCreateToken
                | ModeCreateLine

type Token = Token { id : Int
                   , x : Float 
                   , y : Float 
                   , radius : Float
                   , color : Color 
                   , owner : Int
                   }

type Doodad = DoodadLine { id : Int
                         , sx : Float
                         , sy : Float
                         , ex : Float
                         , ey : Float
                         }

type alias Circle = { x : Float, y : Float, radius : Float } 
type alias Window = { width : Int, height : Int }
type alias User = { id : Int }
type alias Viewport = { x : Float, y : Float, height : Float}
type alias MouseInfo = {x : Float, y : Float}

type alias Model = { tokens : List Token 
                   , doodads : List Doodad
                   , selected : Int
                   , window : Window
                   , user : User 
                   , action : Action
                   , view : Viewport
                   , mouse : MouseInfo
                   , nextId : Int
                   , chat : List (String, String)
                   , chatText : String
                   , sentMessages : Array.Array String
                   , sentMessagePos : Int
                   , username : String
                   , usernameSet : Bool
                   , createMode : CreateMode
                   }

init : (Int, Int) -> (Model,  Cmd Msg)
init (w, h) =
  ({ tokens = []
   , doodads = []
   , selected = -1 
   , window = { width = w, height = h } 
   , user = { id = 0 }
   , action = None
   , view = {x = 0, y = 0, height = 8}
   , mouse = {x = 0, y = 0}
   , nextId = 0
   , chat = [("Server", "Welcome to GOATS ROCK!")]
   , chatText = ""
   , username = ""
   , sentMessages = Array.empty
   , sentMessagePos = 0
   , usernameSet = False
   , createMode = ModeCreateToken
   }, Cmd.none)

screenToWorld : Model -> (Float, Float) -> (Float, Float)
screenToWorld m (x, y) =
  let
    scale = m.view.height / toFloat (computeCanvasHeight m.window.height)
  in
    ((x - 0.5 * toFloat (computeCanvasWidth m.window.width)) * scale + m.view.x
    ,(y - 0.5 * toFloat (computeCanvasHeight m.window.height)) * scale + m.view.y )

worldToScreen : Model -> (Float, Float) -> (Float, Float)
worldToScreen m (x, y) =
  let
    scale =  toFloat (computeCanvasHeight m.window.height) / m.view.height 
  in
    ( (x - m.view.x) * scale + 0.5 * toFloat (computeCanvasWidth m.window.width)
    , (y - m.view.y) * scale + 0.5 * toFloat (computeCanvasHeight m.window.height))

-- Network


type alias RawPacket = {t : String, d : Json.Value}
type Packet = CreateToken Json.Value 
            | DeleteToken Json.Value
            | MoveToken Json.Value
            | Init Json.Value
            | Chat Json.Value
            | CreateDoodadLine Json.Value
            | ClearDoodads Json.Value
            | ClearTokens Json.Value

type alias PacketCreateToken = { x : Float
                               , y : Float
                               }

type alias PacketMoveToken = { id : Int
                             , x : Float
                             , y : Float
                             }

type alias PacketDeleteToken = { id : Int }

type alias InitToken = { id : Int 
                       , x : Float
                       , y : Float
                       , radius : Float
                       , r : Float
                       , g : Float
                       , b : Float
                       }

type alias InitDoodad = { id : Int
                        , doodadType : String
                        , sx : Float
                        , sy : Float
                        , ex : Float
                        , ey : Float
                        }

type alias PacketInit = { tokens : List InitToken
                        , doodads : List InitDoodad
                        , nextColor : Int
                        , nextId : Int }

type alias PacketChat = { sender : String
                        , message : String
                        }

type alias PacketCreateDoodadLine = { sx : Float
                                    , sy : Float
                                    , ex : Float
                                    , ey : Float
                                    }

type alias PacketClearDoodads = {}
type alias PacketClearTokens = {}

initTokenToToken : InitToken -> Token
initTokenToToken t =
  Token
    { id = t.id
    , x = t.x
    , y = t.y
    , radius = t.radius
    , color = Color.rgb255 (round t.r) (round t.g) (round t.b)
    , owner = 0
    }

initDoodadToDoodad : InitDoodad -> Doodad
initDoodadToDoodad t =
  DoodadLine
    { id = t.id
    , sx = t.sx
    , sy = t.sy
    , ex = t.ex
    , ey = t.ey
    }

rawPacketToPacket : RawPacket -> Result String Packet
rawPacketToPacket rawPacket =
  if rawPacket.t == "CreateToken" then
    Ok (CreateToken rawPacket.d)
  else if rawPacket.t == "DeleteToken" then
    Ok (DeleteToken rawPacket.d)
  else if rawPacket.t == "MoveToken" then
    Ok (MoveToken rawPacket.d)
  else if rawPacket.t == "Init" then
    Ok (Init rawPacket.d)
  else if rawPacket.t == "Chat" then
    Ok (Chat rawPacket.d)
  else if rawPacket.t == "CreateDoodadLine" then
    Ok (CreateDoodadLine rawPacket.d)
  else if rawPacket.t == "ClearDoodads" then
    Ok (ClearDoodads rawPacket.d)
  else if rawPacket.t == "ClearTokens" then
    Ok (ClearTokens rawPacket.d)
  else
    Err <| "Unknown packet type " ++ rawPacket.t

decodePacket : Json.Value -> Result String Packet
decodePacket v = 
  let 
    d = Json.map2 RawPacket 
          (Json.field "type" Json.string)
          (Json.field "data" Json.value)
    rawPacket = Json.decodeValue d v
  in
    case rawPacket of
      Ok r -> rawPacketToPacket r
      Err e -> Err <| Json.errorToString e


encodePacket : Packet -> Json.Value
encodePacket p =
  case p of
    CreateToken v -> JE.object [ ("type", JE.string "CreateToken")
                                  , ("data", v)
                                  ]
    DeleteToken v -> JE.object [ ("type", JE.string "DeleteToken")
                                  , ("data", v)
                                  ]
    MoveToken v -> JE.object [ ("type", JE.string "MoveToken")
                                , ("data", v)
                                ]
    Init _ -> JE.object [("type", JE.string "unsupported")]
    Chat v -> JE.object [ ("type", JE.string "Chat")
                        , ("data", v)
                        ]
    CreateDoodadLine v -> JE.object [ ("type", JE.string "CreateDoodadLine")
                                    , ("data", v)
                                    ]
    ClearDoodads v -> JE.object [ ("type", JE.string "ClearDoodads")
                                , ("data", v)
                                ]
    ClearTokens v -> JE.object [ ("type", JE.string "ClearTokens")
                               , ("data", v)
                               ]

decodeCreateToken : Json.Value -> Result String PacketCreateToken 
decodeCreateToken v =
  let
    d = Json.map2 PacketCreateToken
          (Json.field "x" Json.float) 
          (Json.field "y" Json.float) 
    cc = Json.decodeValue d v
  in
    case cc of
      Ok p -> Ok p
      Err e -> Err <| Json.errorToString e


encodeCreateToken : PacketCreateToken -> Json.Value
encodeCreateToken cc = 
  let
    val = JE.object
      [ ("x", JE.float cc.x)
      , ("y", JE.float cc.y)
      ]
    packet = CreateToken val
  in
  encodePacket packet

decodeMoveToken : Json.Value -> Result String PacketMoveToken 
decodeMoveToken v =
  let
    d = Json.map3 PacketMoveToken
          (Json.field "id" Json.int) 
          (Json.field "x" Json.float) 
          (Json.field "y" Json.float) 
    cc = Json.decodeValue d v
  in
    case cc of
      Ok p -> Ok p
      Err e -> Err <| Json.errorToString e

encodeMoveToken : PacketMoveToken -> Json.Value
encodeMoveToken cc = 
  let
    val = JE.object
      [ ("id", JE.int cc.id)
      , ("x", JE.float cc.x)
      , ("y", JE.float cc.y)
      ]
    packet = MoveToken val
  in
  encodePacket packet

decodeDeleteToken : Json.Value -> Result String PacketDeleteToken 
decodeDeleteToken v =
  let
    d = Json.map PacketDeleteToken
          (Json.field "id" Json.int) 
    cc = Json.decodeValue d v
  in
    case cc of
      Ok p -> Ok p
      Err e -> Err <| Json.errorToString e

encodeDeleteToken : PacketDeleteToken -> Json.Value
encodeDeleteToken cc = 
  let
    val = JE.object
      [ ("id", JE.int cc.id)
      ]
    packet = DeleteToken val
  in
  encodePacket packet

decodeInit : Json.Value -> Result String PacketInit
decodeInit v =
  let
    d = Json.map4 PacketInit
          (Json.field "tokens" <| Json.list (
            Json.map7 InitToken
                      (Json.field "id" Json.int)  
                      (Json.field "x" Json.float)  
                      (Json.field "y" Json.float)  
                      (Json.field "radius" Json.float)  
                      (Json.field "r" Json.float)  
                      (Json.field "g" Json.float)  
                      (Json.field "b" Json.float)  

          )) 
          (Json.field "doodads" <| Json.list (
            Json.map6 InitDoodad 
                      (Json.field "id" Json.int)  
                      (Json.field "type" Json.string)  
                      (Json.field "sx" Json.float)  
                      (Json.field "sy" Json.float)  
                      (Json.field "ex" Json.float)  
                      (Json.field "ey" Json.float)  

          )) 
          (Json.field "nextColor" Json.int) 
          (Json.field "nextId" Json.int) 
    cc = Json.decodeValue d v
  in
    case cc of
      Ok p -> Ok p
      Err e -> Err <| Json.errorToString e


decodeChat : Json.Value -> Result String PacketChat 
decodeChat v =
  let
    d = Json.map2 PacketChat
          (Json.field "sender" Json.string) 
          (Json.field "message" Json.string) 
    cc = Json.decodeValue d v
  in
    case cc of
      Ok p -> Ok p
      Err e -> Err <| Json.errorToString e

encodeChat : PacketChat -> Json.Value
encodeChat cc = 
  let
    val = JE.object
      [ ("sender", JE.string cc.sender)
      , ("message", JE.string cc.message)
      ]
    packet = Chat val
  in
  encodePacket packet

decodeCreateDoodadLine : Json.Value -> Result String PacketCreateDoodadLine 
decodeCreateDoodadLine v =
  let
    d = Json.map4 PacketCreateDoodadLine
          (Json.field "sx" Json.float) 
          (Json.field "sy" Json.float) 
          (Json.field "ex" Json.float) 
          (Json.field "ey" Json.float) 
    cc = Json.decodeValue d v
  in
    case cc of
      Ok p -> Ok p
      Err e -> Err <| Json.errorToString e

encodeCreateDoodadLine : PacketCreateDoodadLine -> Json.Value
encodeCreateDoodadLine cc = 
  let
    val = JE.object
      [ ("sx", JE.float cc.sx)
      , ("sy", JE.float cc.sy)
      , ("ex", JE.float cc.ex)
      , ("ey", JE.float cc.ey)
      ]
    packet = CreateDoodadLine val
  in
  encodePacket packet

decodeClearDoodads : Json.Value -> Result String PacketClearDoodads 
decodeClearDoodads _ = Ok PacketClearDoodads

encodeClearDoodads : PacketClearDoodads -> Json.Value
encodeClearDoodads cc = 
  let
    val = JE.object []
    packet = ClearDoodads val
  in
  encodePacket packet

decodeClearTokens : Json.Value -> Result String PacketClearTokens 
decodeClearTokens _ = Ok PacketClearTokens

encodeClearTokens : PacketClearTokens -> Json.Value
encodeClearTokens cc = 
  let
    val = JE.object []
    packet = ClearTokens val
  in
  encodePacket packet

-- Update
type Msg = Move Int (Float, Float)
         | Create (Float, Float)
         | Destroy Int
         | OnResize (Int, Int)
         | MsgInit PacketInit
         | MousePress Mouse.Event
         | MouseMotion Mouse.Event
         | MouseRelease Mouse.Event
         | MouseWheel Wheel.Event
         | KeyDown Keyboard.KeyboardEvent
         | ChatKeyDown Keyboard.KeyboardEvent
         | ChatInput String
         | ChangedFocus (Result Dom.Error ())
         | MsgChat String String
         | MsgSetUsername String
         | MsgFinishUsername 
         | MsgSetCreateMode CreateMode
         | MsgCreateDoodadLine {sx : Float, sy : Float, ex : Float, ey : Float} 
         | MsgSendClearDoodads
         | MsgClearDoodads
         | MsgSendClearTokens
         | MsgClearTokens
         | MsgShowError String
         | MsgDoNothing -- TODO: there is probably a nicer solution for this

-- Apply the given function to all tokens in the list with the given id
applyToToken : (Token -> Token) -> Int -> List Token -> List Token
applyToToken f i l =
  case l of
    [] -> []
    (h::t) ->
      case h of
        Token d ->
          if d.id == i then
            (f h) :: (applyToToken f i t)
          else
            h :: (applyToToken f i t)

deleteToken : Int -> List Token -> List Token
deleteToken i l =
  case l of
    [] -> []
    (h::t) ->
      case h of
        Token d ->
          if d.id == i then
            t
          else
            h :: (deleteToken i t)

getToken : Int -> List Token -> Maybe Token
getToken i l =
  case l of
    [] -> Nothing
    (h::t) ->
      case h of
        Token d ->
          if d.id == i then
            Just <| Token d
          else
            getToken i t

getSelectedPos : Model -> (Float, Float)
getSelectedPos m =
  let
    t = getToken m.selected m.tokens
  in
    case t of
      Just a ->
        case a of
          Token b -> (b.x , b.y)
      Nothing ->
        (0, 0)

  
tokenSetPosition : Float -> Float -> Token -> Token
tokenSetPosition x y t =
  case t of
    Token c -> Token { c | x = x, y = y }

tokenIdAt : Float -> Float -> List Token -> Int
tokenIdAt x y l =
  case l of
    [] -> -1
    (h::t) ->
      case h of
        Token c ->
          if sqrt((x - c.x)^2 + (y - c.y)^2) < c.radius then
            c.id
          else
            tokenIdAt x y t

creatureIdAt : Float -> Float -> List Token -> Int
creatureIdAt x y l =
  case l of
    [] -> -1
    (h::t) ->
      case h of
        Token c ->
          if sqrt((x - c.x)^2 + (y - c.y)^2) < c.radius then
            c.id
          else
            tokenIdAt x y t



onMousePress : Mouse.Event -> Model -> (Model, Cmd Msg)
onMousePress event model =
  let
      (x, y) = screenToWorld model event.offsetPos
  in
    case event.button of
      Mouse.MainButton ->
        let 
          (sx, sy) = event.offsetPos
          s = creatureIdAt x y model.tokens
          mt = getToken s model.tokens
          cx = 
           case mt of 
             Just c ->
               case c of
                 Token cr -> cr.x
             Nothing -> x 
          cy = 
           case mt of 
             Just c ->
               case c of
                 Token cr -> cr.y
             Nothing -> y
          a = if s < 0 then DragView {lastX = sx, lastY = sy}
                       else DragToken {startx = cx, starty = cy
                                      , ox = x - cx, oy = y - cy
                                      , x = cx, y = cy}
          setFocus = Task.attempt ChangedFocus (Dom.focus "canvas")
        in
          if s < 0 && event.keys.ctrl then
            case model.createMode of
              ModeCreateToken ->
                ({ model
                 | action = None
                 , selected = -1
                 }
                , Cmd.batch
                    [ wsSend 
                        (encodeCreateToken (
                          { x = x
                          , y = y
                          }  
                        ))
                    , setFocus])
              ModeCreateLine ->
                ({ model
                 | action = ActionCreateLine { sx = x, sy = y, ex = x, ey = y } 
                 , selected = -1
                 }
                , Cmd.none)
          else
            ({ model |
               selected = s,
               action = a 
             }
            , setFocus)
      Mouse.MiddleButton ->
          ({ model |
             view = {x = 0, y = 0, height = 8} 
           }
          , Cmd.none)
      _ -> (model, Cmd.none)



onMouseRelease : Mouse.Event -> Model -> (Model, Cmd Msg)
onMouseRelease event model =
  let
      (x, y) = screenToWorld model event.offsetPos
  in
    case model.action of
      DragToken d ->
        if sqrt ((d.startx - d.x)^2 + (d.starty - d.y)^2) > 0.1 then
          ({model | action = None},
          wsSend (encodeMoveToken (
              { id = model.selected
              , x = d.x
              , y = d.y
              }
            ))
          )
        else
          ({model | action = None},
          wsSend (encodeMoveToken (
              { id = model.selected
              , x = d.startx
              , y = d.starty
              }
            ))
          )
      ActionCreateLine l ->
        ({model | action = None},
        wsSend (encodeCreateDoodadLine (
                 { sx = l.sx 
                 , sy = l.sy
                 , ex = l.ex
                 , ey = l.ey
                 }
                )))
      _ ->
        ({model | action = None}, Cmd.none)

onMouseMotion : Mouse.Event -> Model -> (Model, Cmd Msg)
onMouseMotion event model =
  let
    (sx, sy) = event.offsetPos
    (x, y) = screenToWorld model event.offsetPos
  in
    case model.action of
      DragToken d ->
        ({ model |
           action = DragToken { startx = d.startx
                              , starty = d.starty
                              , oy = d.oy
                              , ox = d.ox
                              , x = x - d.ox
                              , y = y - d.oy
                              }
         , tokens = applyToToken (tokenSetPosition (x - d.ox) (y - d.oy)) model.selected model.tokens
         }
        , Cmd.none)
      DragView d ->
        ({ model |
           view = { height = model.view.height
                  , x = model.view.x - (sx - d.lastX) / toFloat model.window.height * model.view.height
                  , y = model.view.y - (sy - d.lastY) / toFloat model.window.height * model.view.height}
         , action = DragView {lastX = sx, lastY = sy}
        }
        , Cmd.none)
      ActionCreateLine l ->
        let
          d = sqrt ((x - l.sx)^2 + (y - l.sy)^2) 
        in
          if d > 3 then
            ({ model |
               action = ActionCreateLine { l | sx = l.ex, sy = l.ey, ex = x, ey = y }
             }
            , wsSend (encodeCreateDoodadLine (
                       { sx = l.sx 
                       , sy = l.sy
                       , ex = l.ex
                       , ey = l.ey
                       }
                      )))
          else
            ({ model |
               action = ActionCreateLine { l | ex = x, ey = y }
            }, Cmd.none)
      _ ->
        ({ model|
           mouse = { x = x, y = y }
          }, Cmd.none)


onMouseWheel : Wheel.Event -> Model -> (Model, Cmd Msg)
onMouseWheel event model =
  let
    scrollDir = if event.deltaY > 0 then 1 else -1
  in
    ({ model |
       view = { x = model.view.x
              , y = model.view.y
              , height = model.view.height * (1 + 0.3 * scrollDir)
              }
     }
    , Cmd.none)

onKeyDown: Keyboard.KeyboardEvent -> Model -> (Model, Cmd Msg)
onKeyDown event model =
  if event.keyCode == Key.Delete  &&  model.selected >= 0 then
    ( {model | selected = -1}
    , wsSend (encodeDeleteToken ({id = model.selected})))
  else
    (model, Cmd.none)

onCreate : (Float, Float) -> Model -> (Model, Cmd Msg)
onCreate (x, y) model = 
  ({ model |
     tokens = Token { id = model.nextId
            , x = x 
            , y = y
            , radius = 0.25
            , color = Maybe.withDefault
                        (Color.rgb 0.7 0 0.7)
                        (Array.get
                           (remainderBy numBaseColors model.nextId)
                           baseColors)
            , owner = 0
            } :: model.tokens,
     nextId = model.nextId + 1 
   }
   , Cmd.none)

onDestroy : Int -> Model -> (Model, Cmd Msg)
onDestroy id model =
  ({ model |
     tokens = deleteToken id model.tokens
   }
  , Cmd.none)

onMove : Int -> (Float, Float) -> Model -> (Model, Cmd Msg)
onMove id (x, y) model =
  ({ model |
     tokens = applyToToken (tokenSetPosition x y) id model.tokens
   }
  , Cmd.none)


onMsgInit : PacketInit -> Model -> (Model, Cmd Msg)
onMsgInit d model =
  ({ model |
     nextId = d.nextId
   , tokens = List.map initTokenToToken d.tokens  
   , doodads = List.map initDoodadToDoodad d.doodads
   }
  , Cmd.none)


onChatKeyDown: Keyboard.KeyboardEvent -> Model -> (Model, Cmd Msg)
onChatKeyDown event model =
  if event.keyCode == Key.Enter then 
    ({ model | chatText = ""
     , sentMessages = Array.fromList
                        (List.take 
                          25
                          (model.chatText ::
                            (Array.toList model.sentMessages)))
     , sentMessagePos = 0
     }
    , wsSend (encodeChat ({sender=model.username, message=model.chatText})))
  else if event.keyCode == Key.Up then
    let
      tm = Array.get (model.sentMessagePos) model.sentMessages
      t = Maybe.withDefault model.chatText tm 
    in
      ({ model | chatText = t 
       , sentMessagePos = min (model.sentMessagePos + 1)
                              (Array.length model.sentMessages)
       }
      , Cmd.none)
  else if event.keyCode == Key.Down then
    let
      tm = Array.get (model.sentMessagePos - 2) model.sentMessages
      t = Maybe.withDefault "" tm 
    in
      ({ model | chatText = t 
       , sentMessagePos = max (model.sentMessagePos - 1)
                              (0)
       }
      , Cmd.none)
  else
    (model, Cmd.none)

onMsgChat : String -> String -> Model -> (Model, Cmd Msg)
onMsgChat sender message model =
  ({ model |
   chat = List.take 50 ((sender, message) :: model.chat) 
  }, Cmd.none)

onMsgChatInput : String -> Model -> (Model, Cmd Msg)
onMsgChatInput text model =
  ({ model |
    chatText = text
  }, Cmd.none)


onMsgCreateDoodadLine : (Float, Float) -> (Float, Float) -> Model -> (Model, Cmd Msg)
onMsgCreateDoodadLine (sx, sy) (ex, ey) model =
  ({model |
     doodads = (DoodadLine { id = 0
                           , sx = sx
                           , sy = sy
                           , ex = ex
                           , ey = ey}) :: model.doodads
   }, Cmd.none)

onMsgSendClearDoodads :  Model -> (Model, Cmd Msg)
onMsgSendClearDoodads model =
  ( model
  , wsSend <| encodeClearDoodads PacketClearDoodads
  )

onMsgClearDoodads : Model -> (Model, Cmd Msg)
onMsgClearDoodads model =
  ( { model | doodads = [] }
  , Cmd.none)

onMsgSendClearTokens :  Model -> (Model, Cmd Msg)
onMsgSendClearTokens model =
  ( model
  , wsSend <| encodeClearTokens PacketClearTokens
  )

onMsgClearTokens : Model -> (Model, Cmd Msg)
onMsgClearTokens model =
  ( { model | tokens = [] }
  , Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    Move id (newx, newy) -> onMove id (newx, newy) model
    Create p -> onCreate p model
    Destroy id -> onDestroy id model
    MsgInit d -> onMsgInit d model
    OnResize (w, h) ->
      ({ model | window = {width = w, height = h}}, Cmd.none)
    MsgSetUsername s ->
      ({ model | username = s}, Cmd.none)
    MsgFinishUsername ->
      ({ model | usernameSet = True}, Cmd.none)
    MousePress e -> onMousePress e model
    MouseMotion e -> onMouseMotion e model
    MouseRelease e -> onMouseRelease e model
    MouseWheel e -> onMouseWheel e model
    KeyDown e -> onKeyDown e model
    ChatKeyDown e -> onChatKeyDown e model
    ChangedFocus _ -> (model, Cmd.none)
    MsgChat s m -> onMsgChat s m model
    ChatInput s -> onMsgChatInput s model 
    MsgSetCreateMode m -> ({model | createMode = m}, Cmd.none)
    MsgCreateDoodadLine m -> onMsgCreateDoodadLine (m.sx, m.sy) (m.ex, m.ey)  model
    MsgSendClearDoodads -> onMsgSendClearDoodads model
    MsgClearDoodads -> onMsgClearDoodads model
    MsgSendClearTokens -> onMsgSendClearTokens model
    MsgClearTokens -> onMsgClearTokens model
    MsgDoNothing -> (model, Cmd.none)
    MsgShowError e ->
      let
        v = Debug.log "Error:" e
      in
        (model, Cmd.none)


-- Subscriptions
onCreateToken : JE.Value -> Msg
onCreateToken v =
  let
    p = decodeCreateToken v
  in
    case p of
      Ok o -> Create (o.x, o.y) 
      Err e -> MsgShowError e 

onMoveToken : JE.Value -> Msg
onMoveToken v =
  let
    p = decodeMoveToken v
  in
    case p of
      Ok o -> Move o.id (o.x, o.y) 
      Err e -> MsgShowError e 

onDeleteToken : JE.Value -> Msg
onDeleteToken v =
  let
    p = decodeDeleteToken v
  in
    case p of
      Ok o -> Destroy o.id 
      Err e -> MsgShowError e 

onInit : JE.Value -> Msg
onInit v =
  let
    p = decodeInit v
  in
    case p of
      Ok o -> MsgInit o 
      Err e -> MsgShowError e 

onChat : JE.Value -> Msg
onChat v = 
  let
    p = decodeChat v
  in
    case p of
      Ok o -> MsgChat o.sender o.message 
      Err e -> MsgShowError e 

onCreateDoodadLine : JE.Value -> Msg
onCreateDoodadLine v = 
  let
    p = decodeCreateDoodadLine v
  in
    case p of
      Ok o -> MsgCreateDoodadLine o 
      Err e -> MsgShowError e 


onClearDoodads : JE.Value -> Msg
onClearDoodads v = 
  let
    p = decodeClearDoodads v
  in
    case p of
      Ok o -> MsgClearDoodads
      Err e -> MsgShowError e 

onClearTokens : JE.Value -> Msg
onClearTokens v = 
  let
    p = decodeClearTokens v
  in
    case p of
      Ok o -> MsgClearTokens
      Err e -> MsgShowError e 


onPacket : Packet -> Msg
onPacket p =
  case p of
    CreateToken d -> onCreateToken d
    MoveToken d -> onMoveToken d
    DeleteToken d -> onDeleteToken d
    Init d -> onInit d 
    Chat d -> onChat d 
    CreateDoodadLine d -> onCreateDoodadLine d 
    ClearDoodads d -> onClearDoodads d 
    ClearTokens d -> onClearTokens d 

onWsReceive : JE.Value -> Msg
onWsReceive v =
  let
    p = decodePacket v
  in
    case p of
      Ok o -> onPacket o
      Err e -> MsgShowError e 

subscriptions : Model -> Sub Msg
subscriptions _ = 
  Sub.batch
    [ Browser.Events.onResize onResize 
    , wsReceive onWsReceive
    ]

onResize : Int -> Int -> Msg
onResize w h = OnResize (w, h)

-- Communication
port wsSend : JE.Value -> Cmd msg
port wsReceive : (JE.Value -> msg) -> Sub msg

--View

type alias Dimensions = { canvasWidth : Int
                        , canvasHeight : Int
                        , toolsWidth : Int
                        , toolsHeight : Int
                        , chatWidth : Int
                        , chatHeight : Int
                        , toolbarHeight : Int
                        , toolbarWidth : Int
                        }


computeCanvasHeight : Int -> Int
computeCanvasHeight h = h - 48 

computeCanvasWidth : Int -> Int
computeCanvasWidth w = w - 370

dimensionsFromModel : Model -> Dimensions
dimensionsFromModel m =
  { canvasWidth = computeCanvasWidth m.window.width 
  , canvasHeight = computeCanvasHeight m.window.height
  , toolbarHeight = 40
  , toolbarWidth = (computeCanvasWidth m.window.width) - 30
  , toolsWidth = 350 
  , toolsHeight = floor <| 0 * toFloat m.window.height
  , chatWidth = 350 
  , chatHeight = floor <| 1 * toFloat m.window.height
  }

clearCanvas : Int -> Int -> C.Renderable
clearCanvas w h = C.shapes [Color.rgb 0.2 0.2 0.2 |> C.fill]
                     [C.rect (0, 0) (toFloat w) (toFloat h)]
 
-- Returns the number (i * mul) < a for integer i that is the closest to a
nextHighestMult : Float -> Float -> Float
nextHighestMult a mul =
  toFloat (ceiling (a / mul)) * mul


viewGrid : Float -> Model -> Dimensions -> C.Renderable
viewGrid s m d =
  let
    step = s * toFloat d.canvasHeight / m.view.height
    numx = ceiling (toFloat d.canvasWidth / step)
    numy = ceiling (toFloat d.canvasHeight / step)
    -- Determine the offset that the grid needs
    (minx, miny) = screenToWorld m (0, 0) 
    minxg = nextHighestMult minx s
    minyg = nextHighestMult miny s
    (offxn, offyn) = worldToScreen m (minxg, minyg)
    offx = offxn
    offy = offyn
  in
    C.shapes [Color.rgb 0.3 0.3 0.3 |> C.stroke]
             [ C.path (0, 0)
                      (List.concat
                        (List.map
                          (\ i -> [ C.moveTo ( offx + (toFloat i) * step
                                             , 0.0)
                                  , C.lineTo ( offx + (toFloat i) * step
                                             , toFloat d.canvasHeight)])
                          (List.range 0 numx)))
             , C.path (0, 0)
                      (List.concat
                        (List.map
                          (\ i -> [ C.moveTo ( 0.0
                                             , offy + (toFloat i) * step)
                                  , C.lineTo ( toFloat d.canvasWidth
                                             , offy + (toFloat i) * step)])
                          (List.range 0 numy)))
             ]

viewCircle : Circle -> C.Renderable
viewCircle c =
  C.shapes [Color.rgb 0.4 0 0.7 |> C.fill] [C.circle (c.x, c.y) c.radius]

viewToken : Int -> List C.Setting -> Token -> C.Renderable
viewToken highlighted trans t =
  case t of
    Token d ->
      if highlighted == d.id then
        C.shapes (trans ++ [d.color |> C.fill, Color.rgb 1 1 1 |> C.stroke])
                 [C.circle (d.x, d.y) d.radius]
      else
        C.shapes (trans ++ [d.color |> C.fill])
                 [C.circle (d.x, d.y) d.radius]

viewDoodad : List C.Setting -> Doodad -> C.Renderable
viewDoodad trans t =
  case t of
    DoodadLine l ->
       C.shapes (trans ++ [(Color.rgb 1 1 1) |> C.stroke])
                [C.path (l.sx, l.sy) [C.lineTo (l.ex, l.ey)]]

floatToString : Int -> Float -> String
floatToString d f =
     toFloat (10^d)
  |> (*) f
  |> round
  |> toFloat
  |> (*) (toFloat (10^(-d)))
  |> String.fromFloat


distanceLineRenderable : (Float, Float) -> (Float, Float) -> Model ->
                         Dimensions -> List C.Setting -> List C.Renderable
distanceLineRenderable (sx, sy) (ex, ey) m d trans =
  let
    length = sqrt ((sx - ex)^2 + (sy - ey)^2)
    (ssx, ssy) = worldToScreen m (sx, sy)
    (sex, sey) = worldToScreen m (ex, ey)
  in
    [ C.shapes [Color.rgb 0.5 0.5 0.5 |> C.stroke]
               [ C.path (ssx, ssy) [C.lineTo (sex, sey)]]
    , C.text [ Color.rgb 0.3 0.3 0.3 |> C.stroke
             , Color.rgb 1 1 1 |> C.fill
             , C.font {size = 22, family = "sans serif"}]
             (sex + 10, sey) ((R.round 2 length) ++ "m")
    ]

viewDistanceLine : Model -> Dimensions -> List C.Setting -> List C.Renderable
viewDistanceLine m d trans =
  if m.selected < 0 then []
  else
    case m.action of
      DragToken a ->
        distanceLineRenderable (a.startx, a.starty) (a.x, a.y) m d trans
      _ ->
        let
          (x, y) = getSelectedPos m
        in
          distanceLineRenderable (x, y) (m.mouse.x, m.mouse.y) m d trans

viewCurrentDoodad : List C.Setting -> Model -> List C.Renderable
viewCurrentDoodad trans model =
  case model.action of
    ActionCreateLine l ->
       [C.shapes (trans ++ [(Color.rgb 1 1 1) |> C.stroke])
                 [C.path (l.sx, l.sy) [C.lineTo (l.ex, l.ey)]]]
    _ -> []


canvasTransform : Dimensions -> Model -> List C.Setting
canvasTransform d m =
  let
    scale = toFloat d.canvasHeight / m.view.height
  in
    [ C.transform [ C.translate (toFloat d.canvasWidth / 2)
                                (toFloat d.canvasHeight / 2)
                  , C.scale scale scale 
                  , C.translate -m.view.x -m.view.y
                  ]
    , C.lineWidth <| 2 / scale
    ]

viewChat : Model -> List (Html msg)
viewChat model =
  [Html.dl [] 
    <| List.concat 
    <| List.map (\(s, m) -> [Html.dt [] [Html.text s], Html.dd [] [Html.text m]]) 
    <| List.reverse model.chat
  ]
  
viewSetUsername : Model -> List (Html Msg)
viewSetUsername model =
  if model.usernameSet == False then
    [Html.div [A.id "username-popup"]
              [ Html.text "Please enter a Username:"
              , Html.input [onInput MsgSetUsername] []
              , Html.button [onClick MsgFinishUsername] [Html.text "Ok"]]
    ]
  else
    []

view : Model -> Document Msg
view model =
  let
    dim = dimensionsFromModel model
    trans = canvasTransform dim model
  in
    {title = "Goats Rock"
    , body =
      (List.append [
        Html.div [A.id "main-screen"]
          [ Html.div [ A.id "toolbar"
                     , A.style "height" <| String.fromInt dim.toolbarHeight
                     , A.style "width" <| String.fromInt dim.toolbarWidth
                     ]
                     [ radioButton (MsgSetCreateMode ModeCreateToken)
                                   "images/circle.png" 
                                   <| model.createMode == ModeCreateToken
                     , radioButton (MsgSetCreateMode ModeCreateLine)
                                   "images/line.png"
                                   <| model.createMode == ModeCreateLine

                     , Html.button [A.id "button-clear-doodads"
                                   , onClick MsgSendClearDoodads 
                                   ]
                                   [Html.text "Clear Doodads"]
                     , Html.button [A.id "button-clear-tokens"
                                   , onClick MsgSendClearTokens
                                   ]
                                   [Html.text "Clear Tokens"]
                     ]
          , C.toHtml (dim.canvasWidth, dim.canvasHeight) 
                     [ Mouse.onDown (\event -> MousePress event)
                     , Mouse.onMove (\event -> MouseMotion event)
                     , Mouse.onUp (\event -> MouseRelease event)
                     , Wheel.onWheel (\event -> MouseWheel event)
                     , Html.Events.on "keydown" <|
                         Json.map KeyDown Keyboard.decodeKeyboardEvent 
                     , A.tabindex 0
                     , A.id "canvas"] 
                     (List.concat [ [clearCanvas dim.canvasWidth dim.canvasHeight]
                                  , [viewGrid 5 model dim]
                                  , List.map (viewDoodad trans)
                                             model.doodads
                                  , viewCurrentDoodad trans model
                                  , List.map (viewToken model.selected trans)
                                             model.tokens
                                  , viewDistanceLine model dim trans
                                  ])
          ]
      , Html.div [A.id "right-bar"]
          [ Html.div [ A.id "area-tools"
                     , A.style "height" <| String.fromInt dim.toolsHeight
                     , A.style "width" <| String.fromInt dim.toolsWidth
                     ] []
          , Html.div [ A.id "chat-area"
                     , A.style "height" <| String.fromInt dim.chatHeight
                     , A.style "width" <| String.fromInt dim.chatWidth
                     ] 
            [ Html.div [A.id "chat-text"] (viewChat model)
            , Html.input [A.id "chat-input"
                         , A.value model.chatText
                         , onInput ChatInput 
                         , Html.Events.on "keydown"
                           <| Debug.log "on keydown" <| Json.map ChatKeyDown Keyboard.decodeKeyboardEvent] []]
          ]
      ] (viewSetUsername model))
      
    }

-- View Utils

radioButton : Msg -> String -> Bool -> Html Msg
radioButton m img a =
  Html.label []
             [ Html.input [A.type_ "radio", onInput <| \x -> m, A.checked a] []
             , Html.img [ A.src img ] []
             ]
