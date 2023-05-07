module Row exposing (..)

import Tile exposing (Tile)


type alias Row =
    List Tile


parse : String -> List Tile
parse raw =
    raw
        |> String.split ""
        |> List.map (String.toInt >> Maybe.withDefault 0 >> Tile.create)


moveLeft : Row -> Row
moveLeft =
    List.foldl alignLeft []


alignLeft : Tile -> List Tile -> List Tile
alignLeft tile alreadyMoved =
    recAlignLeft tile
        { left = []
        , right = alreadyMoved ++ [ Tile.zero ]
        }


recAlignLeft : Tile -> { left : List Tile, right : List Tile } -> Row
recAlignLeft toPlace { left, right } =
    case right of
        [] ->
            left

        currentTile :: tail ->
            if currentTile == Tile.zero then
                left ++ (toPlace :: tail)

            else if currentTile == toPlace && not currentTile.summed then
                left ++ (Tile.add currentTile toPlace :: tail)

            else
                recAlignLeft toPlace { left = left ++ [ currentTile ], right = tail }
