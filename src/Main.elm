port module Main exposing (..)

import Browser exposing (Document)
import Browser.Dom as Dom exposing (getViewport)
import Browser.Events
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
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

type Token = Doodad   { id : Int
                      , x : Float
                      , y : Float 
                      , radius : Float
                      , color : Color 
                      }
           | Creature { id : Int
                      , x : Float 
                      , y : Float 
                      , radius : Float
                      , color : Color 
                      , owner : Int
                      }

type alias Circle = { x : Float, y : Float, radius : Float } 
type alias Window = { width : Int, height : Int }
type alias User = { id : Int }
type alias Viewport = { x : Float, y : Float, height : Float}
type alias MouseInfo = {x : Float, y : Float}

type alias Model = { tokens : List Token 
                   , selected : Int
                   , window : Window
                   , user : User 
                   , action : Action
                   , view : Viewport
                   , mouse : MouseInfo
                   , nextId : Int
                   }

init : (Int, Int) -> (Model,  Cmd Msg)
init (w, h) =
  ({ tokens = []
   , selected = -1 
   , window = { width = w, height = h } 
   , user = { id = 0 }
   , action = None
   , view = {x = 0, y = 0, height = 8}
   , mouse = {x = 0, y = 0}
   , nextId = 0
   }, Cmd.none)

screenToWorld : Model -> (Float, Float) -> (Float, Float)
screenToWorld m (x, y) =
  let
    scale = m.view.height / toFloat m.window.height 
  in
    ( (x - 0.8 * 0.5 * toFloat m.window.width) * scale + m.view.x
    , (y - 0.5 * toFloat m.window.height) * scale + m.view.y )

worldToScreen : Model -> (Float, Float) -> (Float, Float)
worldToScreen m (x, y) =
  let
    scale =  toFloat m.window.height / m.view.height 
  in
    ( (x - m.view.x) * scale + 0.8 * 0.5 * toFloat m.window.width
    , (y - m.view.y) * scale + 0.5 * toFloat m.window.height )

-- Network


type alias RawPacket = {t : String, d : Json.Value}
type Packet = CreateCreature Json.Value 
            | DeleteCreature Json.Value
            | MoveCreature Json.Value
            | Init Json.Value

type alias PacketCreateCreature = { x : Float
                                  , y : Float
                                  }

type alias PacketMoveCreature = { id : Int
                                , x : Float
                                , y : Float
                                }

type alias PacketDeleteCreature = { id : Int }

type alias InitToken = { id : Int 
                       , x : Float
                       , y : Float
                       , radius : Float
                       , r : Float
                       , g : Float
                       , b : Float
                       }

type alias PacketInit = { tokens : List InitToken
                        , nextColor : Int
                        , nextId : Int }

initTokenToToken : InitToken -> Token
initTokenToToken t =
  Creature
    { id = t.id
    , x = t.x
    , y = t.y
    , radius = t.radius
    , color = Color.rgb255 (round t.r) (round t.g) (round t.b)
    , owner = 0
    }

rawPacketToPacket : RawPacket -> Result String Packet
rawPacketToPacket rawPacket =
  if rawPacket.t == "CreateCreature" then
    Ok (CreateCreature rawPacket.d)
  else if rawPacket.t == "DeleteCreature" then
    Ok (DeleteCreature rawPacket.d)
  else if rawPacket.t == "MoveCreature" then
    Ok (MoveCreature rawPacket.d)
  else if rawPacket.t == "Init" then
    Ok (Init rawPacket.d)
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
    CreateCreature v -> JE.object [ ("type", JE.string "CreateCreature")
                                  , ("data", v)
                                  ]
    DeleteCreature v -> JE.object [ ("type", JE.string "DeleteCreature")
                                  , ("data", v)
                                  ]
    MoveCreature v -> JE.object [ ("type", JE.string "MoveCreature")
                                , ("data", v)
                                ]
    Init _ -> JE.object [("type", JE.string "unsupported")]

decodeCreateCreature : Json.Value -> Result String PacketCreateCreature 
decodeCreateCreature v =
  let
    d = Json.map2 PacketCreateCreature
          (Json.field "x" Json.float) 
          (Json.field "y" Json.float) 
    cc = Json.decodeValue d v
  in
    case cc of
      Ok p -> Ok p
      Err e -> Err <| Json.errorToString e


encodeCreateCreature : PacketCreateCreature -> Json.Value
encodeCreateCreature cc = 
  let
    val = JE.object
      [ ("x", JE.float cc.x)
      , ("y", JE.float cc.y)
      ]
    packet = CreateCreature val
  in
  encodePacket packet

decodeMoveCreature : Json.Value -> Result String PacketMoveCreature 
decodeMoveCreature v =
  let
    d = Json.map3 PacketMoveCreature
          (Json.field "id" Json.int) 
          (Json.field "x" Json.float) 
          (Json.field "y" Json.float) 
    cc = Json.decodeValue d v
  in
    case cc of
      Ok p -> Ok p
      Err e -> Err <| Json.errorToString e

encodeMoveCreature : PacketMoveCreature -> Json.Value
encodeMoveCreature cc = 
  let
    val = JE.object
      [ ("id", JE.int cc.id)
      , ("x", JE.float cc.x)
      , ("y", JE.float cc.y)
      ]
    packet = MoveCreature val
  in
  encodePacket packet

decodeDeleteCreature : Json.Value -> Result String PacketDeleteCreature 
decodeDeleteCreature v =
  let
    d = Json.map PacketDeleteCreature
          (Json.field "id" Json.int) 
    cc = Json.decodeValue d v
  in
    case cc of
      Ok p -> Ok p
      Err e -> Err <| Json.errorToString e

encodeDeleteCreature : PacketDeleteCreature -> Json.Value
encodeDeleteCreature cc = 
  let
    val = JE.object
      [ ("id", JE.int cc.id)
      ]
    packet = DeleteCreature val
  in
  encodePacket packet

decodeInit : Json.Value -> Result String PacketInit
decodeInit v =
  let
    d = Json.map3 PacketInit
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
          (Json.field "nextColor" Json.int) 
          (Json.field "nextId" Json.int) 
    cc = Json.decodeValue d v
  in
    case cc of
      Ok p -> Ok p
      Err e -> Err <| Json.errorToString e

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
         | ChangedFocus (Result Dom.Error ())
         | MsgShowError String
         | MsgDoNothing -- TODO: there is probably a nicer solution for this

-- Apply the given function to all tokens in the list with the given id
applyToToken : (Token -> Token) -> Int -> List Token -> List Token
applyToToken f i l =
  case l of
    [] -> []
    (h::t) ->
      case h of
        Doodad d ->
          if d.id == i then
            (f h) :: (applyToToken f i t)
          else
            h :: (applyToToken f i t)
        Creature d ->
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
        Doodad d ->
          if d.id == i then
            t
          else
            h :: (deleteToken i t)
        Creature d ->
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
        Doodad d ->
          if d.id == i then
            Just <| Doodad d 
          else
            getToken i t
        Creature d ->
          if d.id == i then
            Just <| Creature d
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
          Doodad b -> (b.x , b.y)
          Creature b -> (b.x , b.y)
      Nothing ->
        (0, 0)

  
tokenSetPosition : Float -> Float -> Token -> Token
tokenSetPosition x y t =
  case t of
    Doodad d -> Doodad { d | x = x, y = y }
    Creature c -> Creature { c | x = x, y = y }

tokenIdAt : Float -> Float -> List Token -> Int
tokenIdAt x y l =
  case l of
    [] -> -1
    (h::t) ->
      case h of
        Doodad d ->
          if sqrt((x - d.x)^2 + (y - d.y)^2) < d.radius then
            d.id
          else
            tokenIdAt x y t
        Creature c ->
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
        Doodad d ->
          tokenIdAt x y t
        Creature c ->
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
                 Creature cr -> cr.x
                 Doodad do -> do.x
             Nothing -> x 
          cy = 
           case mt of 
             Just c ->
               case c of
                 Creature cr -> cr.y
                 Doodad do -> do.y
             Nothing -> y
          a = if s < 0 then DragView {lastX = sx, lastY = sy}
                       else DragToken {startx = cx, starty = cy
                                      , ox = x - cx, oy = y - cy
                                      , x = cx, y = cy}
          setFocus = Task.attempt ChangedFocus (Dom.focus "canvas")
        in
          if s < 0 && event.keys.ctrl then
            ({ model
             | action = None
             , selected = -1
             }
            , Cmd.batch
                [ wsSend 
                    (encodeCreateCreature (
                      { x = x
                      , y = y
                      }  
                    ))
                , setFocus])
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
          wsSend (encodeMoveCreature (
              { id = model.selected
              , x = d.x
              , y = d.y
              }
            ))
          )
        else
          ({model | action = None},
          wsSend (encodeMoveCreature (
              { id = model.selected
              , x = d.startx
              , y = d.starty
              }
            ))
          )

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
    , wsSend (encodeDeleteCreature ({id = model.selected})))
  else
    (model, Cmd.none)

onCreate : (Float, Float) -> Model -> (Model, Cmd Msg)
onCreate (x, y) model = 
  ({ model |
     tokens = Creature { id = model.nextId
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
   }
  , Cmd.none) -- TODO: setup the model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    Move id (newx, newy) -> onMove id (newx, newy) model
    Create p -> onCreate p model
    Destroy id -> onDestroy id model
    MsgInit d -> onMsgInit d model
    OnResize (w, h) ->
      ({ model | window = {width = w, height = h}}, Cmd.none)
    MousePress e -> onMousePress e model
    MouseMotion e -> onMouseMotion e model
    MouseRelease e -> onMouseRelease e model
    MouseWheel e -> onMouseWheel e model
    KeyDown e -> onKeyDown e model
    ChangedFocus _ -> (model, Cmd.none)
    MsgDoNothing -> (model, Cmd.none)
    MsgShowError e ->
      let
        v = Debug.log "Error:" e
      in
        (model, Cmd.none)


-- Subscriptions
onCreateCreature : JE.Value -> Msg
onCreateCreature v =
  let
    p = decodeCreateCreature v
  in
    case p of
      Ok o -> Create (o.x, o.y) 
      Err e -> MsgShowError e 

onMoveCreature : JE.Value -> Msg
onMoveCreature v =
  let
    p = decodeMoveCreature v
  in
    case p of
      Ok o -> Move o.id (o.x, o.y) 
      Err e -> MsgShowError e 

onDeleteCreature : JE.Value -> Msg
onDeleteCreature v =
  let
    p = decodeDeleteCreature v
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

onPacket : Packet -> Msg
onPacket p =
  case p of
    CreateCreature d -> onCreateCreature d
    MoveCreature d -> onMoveCreature d
    DeleteCreature d -> onDeleteCreature d
    Init d -> onInit d 

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
                          (List.range 0 numx)))
             ]

viewCircle : Circle -> C.Renderable
viewCircle c =
  C.shapes [Color.rgb 0.4 0 0.7 |> C.fill] [C.circle (c.x, c.y) c.radius]

viewToken : Int -> List C.Setting -> Token -> C.Renderable
viewToken highlighted trans t =
  case t of
    Doodad d ->
      if highlighted == d.id then
        C.shapes (trans ++ [d.color |> C.fill, Color.rgb 1 1 1 |> C.stroke])
                 [C.circle (d.x, d.y) d.radius]
      else
        C.shapes (trans ++ [d.color |> C.fill])
                 [C.circle (d.x, d.y) d.radius]
    Creature d ->
      if highlighted == d.id then
        C.shapes (trans ++ [d.color |> C.fill, Color.rgb 1 1 1 |> C.stroke])
                 [C.circle (d.x, d.y) d.radius]
      else
        C.shapes (trans ++ [d.color |> C.fill])
                 [C.circle (d.x, d.y) d.radius]

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

dimensionsFromModel : Model -> Dimensions
dimensionsFromModel m =
  { canvasWidth = floor <| 0.8 * toFloat m.window.width
  , canvasHeight = m.window.height
  , toolsWidth = floor <| 0.2 * toFloat m.window.width
  , toolsHeight = floor <| 0.5 * toFloat m.window.height
  , chatWidth = floor <| 0.2 * toFloat m.window.width
  , chatHeight = floor <| 0.5 * toFloat m.window.height
  }

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


view : Model -> Document Msg
view model =
  let
    dim = dimensionsFromModel model
    trans = canvasTransform dim model
  in
    {title = "Goats Rock"
    , body =
      [ C.toHtml (dim.canvasWidth, dim.canvasHeight) 
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
                              , List.map (viewToken model.selected trans)
                                         model.tokens
                              , viewDistanceLine model dim trans
                              ])
      , Html.div [A.id "right-bar"]
          [ Html.div [ A.id "area-tools"
                     , A.style "height" <| String.fromInt dim.toolsHeight
                     , A.style "width" <| String.fromInt dim.toolsWidth
                     ] []
          , Html.div [ A.id "chat-area"
                     , A.style "height" <| String.fromInt dim.chatHeight
                     , A.style "width" <| String.fromInt dim.chatWidth
                     ] 
            [ Html.div [A.id "chat-text"] []
            , Html.input [A.id "chat-input"] []]
          ]
      ]
    }
