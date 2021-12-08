module HomePage exposing (main)

import Browser exposing (..)
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List exposing (head)
import PokeTypes exposing (PokeType(..), allTypes, getColor, getName, listStrongAgainst, listWeakAgainst)
import Random exposing (..)
import Random.List


type alias LastRoundSummary =
    { message : String
    , color : String
    , scoreDelta : Int
    }


type Msg
    = Start
    | Stop
    | CounterWith PokeType
    | RandomizedOpponents (List PokeType)
    | RandomizedStrongAgainst ( Maybe PokeType, List PokeType )
    | RandomizedWeakAgainst ( Maybe PokeType, List PokeType )
    | RandomizedNeutralAgainst ( Maybe PokeType, List PokeType )
    | RandomizeOpponentOrder Int


type State
    = Initial
    | Started


type alias Model =
    { state : State
    , opponent : PokeType
    , score : Int
    , lastRoundSummary : Maybe LastRoundSummary
    , nextOpponentList : List PokeType
    , strongAgainst : PokeType
    , neutralAgainst : PokeType
    , weakAgainst : PokeType
    , counterWith : ( PokeType, PokeType, PokeType )
    }


initialModel : Model
initialModel =
    { state = Initial
    , opponent = Grass
    , score = 0
    , lastRoundSummary = Nothing
    , nextOpponentList = allTypes
    , strongAgainst = ShouldNeverOccur
    , neutralAgainst = ShouldNeverOccur
    , weakAgainst = ShouldNeverOccur
    , counterWith = ( ShouldNeverOccur, ShouldNeverOccur, ShouldNeverOccur )
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
        let
            ( leftCounter, centerCounter, rightCounter ) =
                model.counterWith
        in
        div [ class "game-container" ]
            [ div [ class "title-section" ]
                [ h1 [ class "center" ] [ text "Pokemon Type Practice" ]
                , p [ class "center" ] [ text ("Score: " ++ toString model.score) ]
                , div
                    [ style "display" "flex"
                    , style "justify-content" "center"
                    ]
                    [ case model.lastRoundSummary of
                        Just summary ->
                            div [ class "center"
                            , style "background-color" summary.color 
                            , style "min-width" "300px"
                            , style "min-height" "40px"
                            ] [ text summary.message ]

                        Nothing ->
                            div [] []
                    ]
                ]
            , div
                [ class "opponent center"
                , style "margin" "30px"
                , style "background-color" (getColor model.opponent)
                ]
                [ div []
                    [ p [] [ text "Opponent is:" ]
                    , p [ class "type-name" ] [ text (getName model.opponent) ]
                    ]
                ]
            , counterDiv "verses1 center" leftCounter
            , counterDiv "verses2 center" centerCounter
            , counterDiv "verses3 center" rightCounter
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


rotateOppponents : List PokeType -> ( PokeType, List PokeType )
rotateOppponents opponentList =
    let
        head =
            case List.head opponentList of
                Just h ->
                    h

                Nothing ->
                    ShouldNeverOccur

        tail =
            case List.tail opponentList of
                Just l ->
                    l

                Nothing ->
                    []
    in
    ( head, tail ++ [ head ] )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( { model | state = Started }, Cmd.none )

        Stop ->
            ( { model | state = Initial }, Cmd.none )

        CounterWith counterWith ->
            let
                lastRoundSummary =
                    generateLastRoundSummary model.opponent counterWith

                ( nextOpponent, nextOpponentList ) =
                    rotateOppponents model.nextOpponentList
            in
            ( { model
                | opponent = nextOpponent
                , nextOpponentList = nextOpponentList
                , score = model.score + lastRoundSummary.scoreDelta
                , lastRoundSummary = Just lastRoundSummary
              }
            , startOpponentSelection
            )

        RandomizedOpponents opponentList ->
            let
                ( nextOpponent, nextOpponentList ) =
                    rotateOppponents opponentList

                strongAgainstList =
                    listStrongAgainst nextOpponent
            in
            ( { model | opponent = nextOpponent, nextOpponentList = nextOpponentList }
            , generate RandomizedStrongAgainst (Random.List.choose strongAgainstList)
            )

        RandomizedStrongAgainst ( strongAgainstMaybe, _ ) ->
            let
                strongAgainst =
                    case strongAgainstMaybe of
                        Just pokeType ->
                            pokeType

                        Nothing ->
                            model.opponent

                weakAgainstList =
                    listWeakAgainst model.opponent
            in
            ( { model | strongAgainst = strongAgainst }
            , generate RandomizedWeakAgainst (Random.List.choose weakAgainstList)
            )

        RandomizedWeakAgainst ( weakAgainstMaybe, _ ) ->
            let
                weakAgainst =
                    case weakAgainstMaybe of
                        Just pokeType ->
                            pokeType

                        Nothing ->
                            model.opponent

                neutralAgainstList =
                    listNeutalAgainst model.opponent allTypes
            in
            ( { model | weakAgainst = weakAgainst }
            , generate RandomizedNeutralAgainst (Random.List.choose neutralAgainstList)
            )

        RandomizedNeutralAgainst ( neutralAgainstMaybe, _ ) ->
            let
                neutralAgainst =
                    case neutralAgainstMaybe of
                        Just pokeType ->
                            pokeType

                        Nothing ->
                            model.opponent
            in
            ( { model | neutralAgainst = neutralAgainst }
            , generate RandomizeOpponentOrder (Random.int 1 6)
            )

        RandomizeOpponentOrder randomInt ->
            let
                counterWith =
                    case randomInt of
                        1 ->
                            ( model.strongAgainst, model.weakAgainst, model.neutralAgainst )

                        2 ->
                            ( model.strongAgainst, model.neutralAgainst, model.weakAgainst )

                        3 ->
                            ( model.weakAgainst, model.strongAgainst, model.neutralAgainst )

                        4 ->
                            ( model.weakAgainst, model.neutralAgainst, model.strongAgainst )

                        5 ->
                            ( model.neutralAgainst, model.weakAgainst, model.strongAgainst )

                        6 ->
                            ( model.neutralAgainst, model.strongAgainst, model.weakAgainst )

                        _ ->
                            ( ShouldNeverOccur, ShouldNeverOccur, ShouldNeverOccur )
            in
            ( { model | counterWith = counterWith }
            , Cmd.none
            )


generateLastRoundSummary : PokeType -> PokeType -> LastRoundSummary
generateLastRoundSummary opponentType myCounterType =
    let
        isCounterStrong =
            List.member myCounterType (listStrongAgainst opponentType)

        isCounterWeak =
            List.member myCounterType (listWeakAgainst opponentType)

        ( color, scoreDelta ) =
            case ( isCounterStrong, isCounterWeak ) of
                ( True, True ) ->
                    ( "yellow", 0 )

                ( False, False ) ->
                    ( "yellow", 0 )

                ( True, False ) ->
                    ( "red", -10 )

                ( False, True ) ->
                    ( "green", 10 )
    in
    { message = getName myCounterType ++ " vs " ++ getName opponentType, color = color, scoreDelta = scoreDelta }


startOpponentSelection : Cmd Msg
startOpponentSelection =
    generate RandomizedOpponents (Random.List.shuffle allTypes)


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, startOpponentSelection )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none


listNeutalAgainst : PokeType -> List PokeType -> List PokeType
listNeutalAgainst pokeType allTypes =
    let
        allWeakAndStrongAndSelf =
            pokeType :: listWeakAgainst pokeType ++ listStrongAgainst pokeType
    in
    List.filter (\pt -> not (List.member pt allWeakAndStrongAndSelf)) allTypes
