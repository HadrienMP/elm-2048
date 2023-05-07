module Grid exposing (..)

import Row exposing (Row)
import Tile exposing (Tile)


type alias Grid =
    List Row


type Move
    = Left


parseGrid : List String -> Grid
parseGrid =
    List.map Row.parse


printGrid : Grid -> List String
printGrid grid =
    grid
        |> List.map
            (List.foldr
                (\tile acc -> String.fromInt tile.face ++ acc)
                ""
            )


handle : Move -> Grid -> Grid
handle _ grid =
    grid |> List.map Row.moveLeft
