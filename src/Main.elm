import Browser exposing (Document)
import Browser.Dom exposing (getViewport)
import Browser.Events
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html.Attributes as A
import Canvas  as C
import Canvas.Settings as C
import Canvas.Settings.Advanced as C
import Canvas.Settings.Line as C
import Color exposing (Color)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Wheel as Wheel
import List
import String
import Json.Decode as Json
import Debug
import Round as R

-- Setup
main =
  Browser.document { init = init, update = update, subscriptions = subscriptions, view = view }

-- Model

gmId : Int
gmId = 0

type Action = None
            | DragToken {startx : Float, starty : Float, x : Float, y : Float}
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
                   }

init : (Int, Int) -> (Model,  Cmd Msg)
init (w, h) =
  ({ tokens = [Creature { id = 0
                        , x = 0
                        , y = 0
                        , radius = 0.5
                        , color = Color.rgb 0.7 0 0.7
                        , owner = 0 }]
   , selected = -1 
   , window = { width = w, height = h } 
   , user = { id = 0 }
   , action = None
   , view = {x = 0, y = 0, height = 8}
   , mouse = {x = 0, y = 0}
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

-- Update
type Msg = Move Int (Float, Float)
         | OnResize (Int, Int)
         | MousePress Mouse.Event
         | MouseMotion Mouse.Event
         | MouseRelease Mouse.Event
         | MouseWheel Wheel.Event

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
          a = if s < 0 then DragView {lastX = sx, lastY = sy}
                       else DragToken {startx = x, starty = y, x = x, y = y}
        in
          ({ model |
             selected = s,
             action = a 
           }
          , Cmd.none)
      Mouse.MiddleButton ->
          ({ model |
             view = {x = 0, y = 0, height = 8} 
           }
          , Cmd.none)
      _ -> (model, Cmd.none)



onMouseRelease : Mouse.Event -> Model -> (Model, Cmd Msg)
onMouseRelease _ model =
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
                              , x = x
                              , y = y
                              }
         , tokens = applyToToken (tokenSetPosition x y) model.selected model.tokens
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
  ({ model |
     view = { x = model.view.x
            , y = model.view.y
            , height = model.view.height * (1 + 0.1 * event.deltaY)
            }
   }
  , Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    Move id (newx, newy) ->
      ({ model |
         tokens = applyToToken (tokenSetPosition newx newy) id model.tokens
       }
      , Cmd.none)
    OnResize (w, h) ->
      ({ model | window = {width = w, height = h}}, Cmd.none)
    MousePress e -> onMousePress e model
    MouseMotion e -> onMouseMotion e model
    MouseRelease e -> onMouseRelease e model
    MouseWheel e -> onMouseWheel e model


-- Subscriptions
subscriptions : Model -> Sub Msg
subscriptions _ = 
  Browser.Events.onResize onResize 

onResize : Int -> Int -> Msg
onResize w h = OnResize (w, h)

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
    , C.text [Color.rgb 0.7 0.7 0.7 |> C.stroke]
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
