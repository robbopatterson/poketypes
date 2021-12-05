module HomePage exposing (main)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Task exposing (onError)


type Msg
    = Start
    | Stop


type State
    = Initial
    | Started
    | Done


type alias Model =
    { state : State }


initialModel =
    Model Initial


view model =
    div [ class "jumbotron" ]
        [ h1 [] [ text "Welcome to PokeTypes!" ]
        , p [] [ text "Choose as many best Counters as you can in 30 seconds." ]
        , a [ href "https://www.pinterest.ca/pin/329396160253653038/" ] [ text "Click for Chart" ]
        , p [] []
        , if model.state == Started then
            div []
                [ text "Started"
                , button [ onClick Stop ] [ text "Stop" ]
                ]

          else
            div []
                [ text "Not Started"
                , button [ onClick Start ] [ text "Start" ]
                ]
        ]


update msg model =
    case msg of
        Start ->
            { model | state = Started }
        Stop ->
            { model | state = Initial }


main =
    Browser.sandbox { init = initialModel, update = update, view = view }
