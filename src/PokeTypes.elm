module PokeTypes exposing (..)

type PokeType = 
    Fairy
    | Steel
    | Dark
    | Dragon

allTypes : List PokeType
allTypes = [
    Fairy
    ,Steel
    ,Dark
    ,Dragon
    ]

getName : PokeType -> String
getName pokeType =
    case pokeType of
       Fairy -> "Fairy"
       Steel -> "Steel"
       Dark -> "Dark"
       Dragon -> "Dragon"

getVs: PokeType -> List PokeType
getVs pokeType =
    case pokeType of
        Fairy -> [ Steel, Dragon, Dark ]
        Steel -> [ Fairy, Dragon, Dark ]
        Dragon -> [ Fairy, Steel, Dark ]
        Dark -> [ Fairy, Dragon, Steel ]
  