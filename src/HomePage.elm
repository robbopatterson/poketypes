module HomePage exposing (main)

import Browser exposing (..)
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List exposing (head)
import PokeTypes exposing (PokeType(..), getColor, getName, listStrongAgainst, listWeakAgainst)
import PokeTypes exposing (allTypes)
import Random exposing(..)

type alias LastRoundSummary = {
        message: String
        ,color: String
        ,scoreDelta: Int
    }

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
    , score : Int
    , lastRoundSummary : Maybe LastRoundSummary
    , nextOpponentList : List PokeType
    }


initialModel : Model
initialModel = {
    state = Initial,
    opponent = Grass, 
    score = 0, 
    lastRoundSummary=Nothing,
    nextOpponentList=allTypes
    }


view : Model -> Html Msg
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
                , p [ class "center" ] [ text ("Score: " ++ toString model.score) ]
                , div [] [
                    case model.lastRoundSummary of 
                        Just summary -> 
                            div [ class "center", style "background-color" summary.color ] [ text summary.message ]
                        Nothing ->
                            div [] []
                ]
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


rotateOppponents: List PokeType -> ( PokeType, List PokeType )
rotateOppponents opponentList = 
    let
        head = case List.head opponentList of 
            Just h -> h 
            Nothing -> ShouldNeverOccur
        tail = case List.tail opponentList of
            Just l -> l
            Nothing -> []
    in
        (head, tail ++ [head]) 

update : Msg -> Model -> (Model,Cmd msg)
update msg model =
    case msg of
        Start ->
            ( { model | state = Started }, Cmd.none )

        Stop ->
            ( { model | state = Initial }, Cmd.none )

        CounterWith counterWith ->
            let
                lastRoundSummary = generateLastRoundSummary model.opponent counterWith
                (nextOpponent, nextOpponentList) = rotateOppponents model.nextOpponentList
            in
            ( { model
                | opponent = nextOpponent
                , nextOpponentList = nextOpponentList
                , score = model.score + lastRoundSummary.scoreDelta
                , lastRoundSummary = Just lastRoundSummary
            }, Cmd.none )

generateLastRoundSummary: PokeType -> PokeType -> LastRoundSummary
generateLastRoundSummary opponentType myCounterType =
    let
        isCounterStrong = List.member myCounterType (listStrongAgainst opponentType)
        isCounterWeak = List.member myCounterType (listWeakAgainst opponentType)

        (color, scoreDelta) = case (isCounterStrong, isCounterWeak) of
           (True,True) -> ("yellow",0)
           (False,False) -> ("yellow",0)
           (True,False) -> ("red",-10)
           (False,True) -> ("green",10)
    in
        { message = (getName myCounterType) ++ " vs " ++ (getName opponentType), color=color, scoreDelta=scoreDelta }

main : Program () Model Msg
main =
    Browser.element { 
        init = \flags->(initialModel,Cmd.none), 
        update = update, 
        view = view, 
        subscriptions=subscriptions 
        }

subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none

getCounter : Int -> PokeType -> PokeType
getCounter position pokeType =
    let
        vsList =
            getVersesList pokeType
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


getVersesList : PokeType -> List PokeType
getVersesList pokeType =
    let
        weakAgainst =
            case listWeakAgainst pokeType |> List.head of
                Just pt ->
                    pt

                Nothing ->
                    pokeType

        strongAgainst =
            case listStrongAgainst pokeType |> List.head of
                Just pt ->
                    pt

                Nothing ->
                    pokeType

        neutralAgainst =
            pokeType
    in
    [ weakAgainst, neutralAgainst, strongAgainst ]
