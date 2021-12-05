module HomePage exposing (main)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Task exposing (onError)
import PokeTypes exposing (PokeType(..),getName)


type Msg
    = Start
    | Stop
    | ChooseDefender PokeType

type State
    = Initial
    | Started


type alias Model =
    { state : State
    , opponent: PokeType }


initialModel : Model
initialModel =
    { state = Initial, opponent = Fairy }


view model =
    if model.state == Initial then
        div [ class "jumbotron" ]
            [ h1 [] [ text "Welcome to PokeTypes!" ]
            , p [] [ text "Choose as many best Counters as you can in 30 seconds." ]
            , a [ href "https://www.pinterest.ca/pin/329396160253653038/" ] [ text "Click for Chart" ]
            , p [] []
            , button [ onClick Start ] [ text "Start" ]
            ]

    else
        div [ class "game-container" ]
            [ div [ class "title-section" ] [
                h1 [ class "center" ] [text "Pokemon Type Practice"]
                , p [class "center"] [text "Score: 0"]
                ]
            , div [ class "opponent center" ] [ 
                div [] [
                    p [] [ text "Opponent is:"]
                    ,p [ class "type-name"] [ text (getName model.opponent)]
                ]
                ]
            , div [ class "verses1 center" ] [ text "1" ]
            , div [ class "verses2 center" ] [ text "2" ]
            , div [ class "verses3 center" ] [ text "3" ]
            ]


update msg model =
    case msg of
        Start ->
            { model | state = Started }

        Stop ->
            { model | state = Initial }

        ChooseDefender counterWith -> 
            { model | opponent = counterWith }

main =
    Browser.sandbox { init = initialModel, update = update, view = view }
