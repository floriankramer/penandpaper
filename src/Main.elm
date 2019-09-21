import Browser exposing (Document)
import Browser.Dom exposing (getViewport)
import Browser.Events
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html.Attributes as A
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Color exposing (Color)
import Html.Events.Extra.Mouse as Mouse
import List
import String

-- Setup
main =
  Browser.document { init = init, update = update, subscriptions = subscriptions, view = view }

-- Model

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
type alias Window = { width : Int, height : Int }

type alias Model = { tokens : List Circle
                   , window : Window
                   }

init : (Int, Int) -> (Model,  Cmd Msg)
init (w, h) = ({ tokens = [{ x = 70, y = 70, radius = 50 }]
          , window = { width = w, height = h } 
          }, Cmd.none)

-- Update
type Msg = Move (Float, Float)
         | OnResize (Int, Int)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    Move (newx, newy) ->
      (model, Cmd.none)
    OnResize (w, h) ->
      ({ model | window = {width = w, height = h}}, Cmd.none)


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

clearCanvas : Int -> Int -> Renderable
clearCanvas w h = shapes [Color.rgb 0.2 0.2 0.2 |> fill]
                     [rect (0, 0) (toFloat w) (toFloat h)]

viewCircle : Circle -> Renderable
viewCircle c =
  shapes [Color.rgb 0.4 0 0.7 |> fill] [circle (c.x, c.y) c.radius]

dimensionsFromModel : Model -> Dimensions
dimensionsFromModel m =
  { canvasWidth = floor <| 0.8 * toFloat m.window.width
  , canvasHeight = m.window.height
  , toolsWidth = floor <| 0.2 * toFloat m.window.width
  , toolsHeight = floor <| 0.5 * toFloat m.window.height
  , chatWidth = floor <| 0.2 * toFloat m.window.width
  , chatHeight = floor <| 0.5 * toFloat m.window.height
  }

view : Model -> Document Msg
view model =
  let
    dim = dimensionsFromModel model
  in
    {title = "Goats Rock"
    , body =
      [ Canvas.toHtml (dim.canvasWidth, dim.canvasHeight) 
        [ Mouse.onDown (\event -> Move event.offsetPos)
        , A.id "canvas"] 
        (clearCanvas dim.canvasWidth dim.canvasHeight ::
          (List.map viewCircle model.tokens))
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
--  let
--    body = 
--  in
--    {title = "Goats Rock", body = body}
