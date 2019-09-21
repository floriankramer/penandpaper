import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Color exposing (Color)
import Html.Events.Extra.Mouse as Mouse

-- Setup
main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }

-- Model
canvasHeight : Int
canvasHeight = 500

canvasWidth : Int
canvasWidth = 500

type Token = Doodad   { id : Int
                      , x : Float
                      , y : Float 
                      , radius : Float
                      , color : Color 
                      }
           | Creature { id : Int
                      , x : Int
                      , y : Float 
                      , radius : Float
                      , color : Color 
                      }

type alias Circle = { x : Float, y : Float, radius : Float } 

type alias Model = Circle 

init : () -> (Model,  Cmd Msg)
init _ = ({ x = 70, y = 70, radius = 50 }, Cmd.none)

-- Update
type Msg = Move (Float, Float)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    Move (newx, newy) ->
      ({ model | x = newx, y = newy}, Cmd.none)

-- Subscriptions
subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

--View
clearCanvas : Renderable
clearCanvas = shapes [Color.rgb 0.2 0.2 0.2 |> fill]
                     [rect (0, 0) (toFloat canvasWidth) (toFloat canvasHeight)]

view model =
   Canvas.toHtml (canvasWidth, canvasHeight) 
      [Mouse.onDown (\event -> Move event.offsetPos)] 
      [ clearCanvas
      , shapes [ Color.rgb 0.4 0 0.7 |> fill
               ]
               [ circle (model.x, model.y) model.radius
               ]
      ]
