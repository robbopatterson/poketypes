module HomePage exposing (main)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List exposing (head)
import PokeTypes exposing (PokeType(..), getColor, getName, getVs)


type Msg
    = Start
    | Stop
    | CounterWith PokeType


type State
    = Initial
    | Started


type alias Model =
    { state : State
    , opponent : PokeType
    }


initialModel : Model
initialModel =
    { state = Initial, opponent = Fairy }


view : { a | state : State, opponent : PokeType } -> Html Msg
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
            [ div [ class "title-section" ]
                [ h1 [ class "center" ] [ text "Pokemon Type Practice" ]
                , p [ class "center" ] [ text "Score: 0" ]
                ]
            , div
                [ class "opponent center"
                , style "background-color" (getColor model.opponent)
                ]
                [ div []
                    [ p [] [ text "Opponent is:" ]
                    , p [ class "type-name" ] [ text (getName model.opponent) ]
                    ]
                ]
            , counterDiv "verses1 center" (leftCounter model)
            , counterDiv "verses2 center" (centerCounter model)
            , counterDiv "verses3 center" (rightCounter model)
            ]


counterDiv : String -> PokeType -> Html Msg
counterDiv classStr pokeType =
    div
        [ class classStr
        , style "background-color" (getColor pokeType)
        , onClick (CounterWith pokeType)
        ]
        [ div []
            [ p [] [ text "Counter with:" ]
            , p [ class "type-name" ] [ text (getName pokeType) ]
            ]
        ]


leftCounter model =
    getCounter 0 model.opponent


centerCounter model =
    getCounter 1 model.opponent


rightCounter model =
    getCounter 2 model.opponent


update msg model =
    case msg of
        Start ->
            { model | state = Started }

        Stop ->
            { model | state = Initial }

        CounterWith counterWith ->
            { model | opponent = counterWith }


main =
    Browser.sandbox { init = initialModel, update = update, view = view }


getCounter : Int -> PokeType -> PokeType
getCounter position pokeType =
    let
        vsList =
            getVs pokeType
    in
    getAtPositionOrDefault position vsList pokeType


getAtPositionOrDefault : Int -> List a -> a -> a
getAtPositionOrDefault position list default =
    let
        remainingList =
            List.drop position list

        head =
            List.head remainingList
    in
    case head of
        Just h ->
            h

        _ ->
            default
