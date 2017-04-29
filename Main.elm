module Main exposing (..)

import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Random
import Array exposing (Array)
import Time exposing (Time)

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type alias Point =
    { x : Float
    , y : Float
    }

type alias Model =
    { vertices : Array Point
    , points : List Point
    }

initModel : Model
initModel =
    { vertices = 
        Array.fromList [ { x = 250, y = 10 }
                       , { x = 10, y = 490 }
                       , { x = 490, y = 490 }
                       ]
    , points = 
        [ { x = 250, y = 250 } ]
    }

init : (Model, Cmd Msg)
init =
    (initModel, Cmd.none)


type Msg
    = Tick Time
    | RollResult Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick time ->
            (model,  Random.generate RollResult <| Random.int 0 <| Array.length model.vertices - 1)

        RollResult x ->
            let
                vertex = Array.get x model.vertices
                lastPoint = List.head model.points
            in
                case (vertex, lastPoint) of
                    (Just v, Just p) ->
                        let
                            dx = abs (v.x - p.x) / 2
                            dy = abs (v.y - p.y) / 2

                            newPoint =
                                { x = if p.x >= v.x then p.x - dx else p.x + dx
                                , y = if p.y >= v.y then p.y - dy else p.y + dy
                                }
                            newModel =
                                { model |
                                    points = newPoint :: model.points
                                }
                        in 
                            (newModel, Cmd.none)

                    (_, _) ->
                        (model, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every Time.millisecond Tick


view : Model -> Html Msg
view model =
    div []
        [ svg [ width "500", height "500", viewBox "0 0 500 500", fill "black" ]
              [ rect [width "500", height "500", fill "black" ] []
              , viewPoints "red" "2" <| Array.toList model.vertices
              , viewPoints "white" "0.5" model.points
              ]
        ]

viewPoints : String -> String -> List Point -> Svg Msg
viewPoints color radius points =
    let
        pointToCircle p =
            circle [cx (toString p.x), cy (toString p.y), r radius] []
    in
        g [fill color]  <| List.map pointToCircle points
