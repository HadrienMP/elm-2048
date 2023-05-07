module Tile exposing (..)


type alias Tile =
    { face : Int
    , summed : Bool
    }


create : Int -> Tile
create face =
    Tile face False


add : Tile -> Tile -> Tile
add a b =
    { face = a.face + b.face, summed = True }


zero : Tile
zero =
    { face = 0, summed = False }
