module Row exposing (Row, moveLeft, moveRight)

import Tile exposing (Tile)


type alias Row =
    List Int


moveRight : Row -> Row
moveRight =
    List.reverse
        >> moveLeft
        >> List.reverse


moveLeft : Row -> Row
moveLeft =
    List.map Tile.create
        >> List.foldl alignLeft []
        >> List.map .face


alignLeft : Tile -> List Tile -> List Tile
alignLeft tile alreadyMoved =
    recAlignLeft tile
        { left = []
        , right = alreadyMoved ++ [ Tile.zero ]
        }


recAlignLeft : Tile -> { left : List Tile, right : List Tile } -> List Tile
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
