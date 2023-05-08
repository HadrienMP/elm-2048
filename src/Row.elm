module Row exposing (Row, moveLeft, moveRight)

import Tile exposing (Tile)


type alias Row =
    List Tile


parseRec : String -> List Tile -> List Tile
parseRec raw acc =
    case String.uncons raw of
        Just ( head, tail ) ->
            Char.toCode head |> (-) 48 |> abs |> Tile.create |> List.singleton |> (++) acc |> parseRec tail

        Nothing ->
            acc


moveRight : List Tile -> List Tile
moveRight =
    List.reverse >> moveLeft >> List.reverse


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
