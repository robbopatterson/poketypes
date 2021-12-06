module PokeTypes exposing (..)


type PokeType
    = Fairy
    | Steel
    | Dark
    | Dragon
    | Grass
    | Water
    | Ground
    | Rock
    | Fire
    | Ice
    | Poison
    | Flying
    | Bug
    | Psychic
    | Fighting
    | Ghost
    | Electric
    | Normal
    | ShouldNeverOccur

allTypes : List PokeType
allTypes =
    [ Fairy
    , Steel
    , Dark
    , Dragon
    , Grass
    , Water
    , Ground
    , Rock
    , Fire
    , Ice
    , Poison
    , Flying
    , Bug
    , Psychic
    , Fighting
    , Ghost
    , Electric
    , Normal
    ]
-- NOTE: ShouldNeverOccur is not listed in allTypes


getName : PokeType -> String
getName pokeType =
    case pokeType of
        Grass ->
            "Grass"

        Fairy ->
            "Fairy"

        Steel ->
            "Steel"

        Dark ->
            "Dark"

        Dragon ->
            "Dragon"

        Water ->
            "Water"

        Ground ->
            "Ground"

        Rock ->
            "Rock"

        Fire ->
            "Fire"

        Ice ->
            "Ice"

        Poison ->
            "Poison"

        Flying ->
            "Flying"

        Bug ->
            "Bug"

        Psychic ->
            "Psychic"

        Fighting ->
            "Fight"

        Ghost ->
            "Ghost"

        Electric ->
            "Electric"

        Normal ->
            "Normal"

        ShouldNeverOccur ->
            "INTERNAL Error.  This should have be used."

getColor : PokeType -> String
getColor pokeType =
    case pokeType of
        Grass ->
            "#77C850"

        Fairy ->
            "#DEA4DF"

        Steel ->
            "#B8B8D0"

        Dark ->
            "#705848"

        Dragon ->
            "#7039F9"

        Water ->
            "#6890EF"

        Ground ->
            "#E1BF68"

        Rock ->
            "#B8A037"

        Fire ->
            "#F1802F"

        Ice ->
            "#98D8D8"

        Poison ->
            "#A0409F"

        Flying ->
            "#A891F0"

        Bug ->
            "#A8B820"

        Psychic ->
            "#F85888"

        Fighting ->
            "#C03028"

        Ghost ->
            "#A0409F"

        Electric ->
            "#F8D030"

        Normal ->
            "#A7A878"

        ShouldNeverOccur ->
            ""


listStrongAgainst : PokeType -> List PokeType
listStrongAgainst pokeType =
    case pokeType of
        Grass ->
            [ Water, Ground, Rock ]

        Rock ->
            [ Fire, Ice, Flying, Bug ]

        Ice ->
            [ Grass, Ground, Flying, Dragon ]

        Dragon ->
            [ Dragon ]

        Dark ->
            [ Psychic ]

        Psychic ->
            [ Fighting, Poison ]

        Bug ->
            [ Grass, Psychic, Dark ]

        Flying ->
            [ Grass, Fighting, Bug ]

        Steel ->
            [ Ice, Rock, Fairy ]

        Fire ->
            [ Grass, Ice, Bug, Steel ]

        Fighting ->
            [ Normal, Ice, Rock, Dark, Steel ]

        Ground ->
            [ Fire, Electric, Poison, Rock, Steel ]

        Ghost ->
            [ Psychic, Ghost ]

        Poison ->
            [ Grass, Fairy ]

        Water ->
            [ Fire, Ground, Rock ]

        Fairy ->
            [ Fighting, Dragon, Dark ]

        Electric ->
            [ Water, Flying ]

        Normal ->
            []

        ShouldNeverOccur ->
            []


listWeakAgainst : PokeType -> List PokeType
listWeakAgainst pokeType =
    case pokeType of
        Grass ->
            [ Fire, Ice, Poison, Flying, Bug ]

        Rock ->
            [Water, Grass, Fighting, Ground, Steel]
            
        Ice ->
            [Fire, Fighting, Rock, Steel]
            
        Dragon ->
            [Ice, Dragon, Fairy]
            
        Dark ->
            [Fighting, Bug, Fairy]
            
        Psychic ->
            [Bug,Ghost,Dark]
            
        Bug ->
            [Fire,Flying,Rock]
            
        Flying ->
            [Electric,Ice,Rock]
            
        Steel ->
            [Fire,Fighting,Ground]
            
        Fire ->
            [Water,Rock,Ground]
            
        Fighting ->
            [Flying,Psychic,Fairy]
            
        Ground ->
            [Water,Grass,Ice]
            
        Ghost ->
            [Ghost,Dark]
            
        Poison ->
            [Ground,Psychic]
            
        Water ->
            [Electric,Grass]
            
        Fairy ->
            [Poison,Steel]
            
        Electric ->
            [Ground]

        Normal ->
            [Fighting]

        ShouldNeverOccur ->
            []
