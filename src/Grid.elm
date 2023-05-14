module Grid exposing (Coordinates, Grid, RandomTile, handle, init, listAvailableSquares, randomTileGenerator, turnClockwise, turnCounterClockwise)

import Move exposing (Move)
import Random
import Random.List
import Row exposing (Row)


type alias Grid =
    List Row



-- Create


init : Grid
init =
    0
        |> List.repeat 4
        |> List.repeat 4



-- Handle


handle : Move -> Grid -> Grid
handle move =
    case move of
        Move.Left ->
            List.map Row.moveLeft

        Move.Right ->
            List.map Row.moveRight

        Move.Down ->
            turnClockwise >> List.map Row.moveLeft >> turnCounterClockwise

        Move.Up ->
            turnCounterClockwise >> List.map Row.moveLeft >> turnClockwise



-- Turn grids


turnCounterClockwise : Grid -> Grid
turnCounterClockwise original =
    original
        |> List.foldr
            (\row -> concat (row |> List.map List.singleton |> List.reverse))
            []


turnClockwise : Grid -> Grid
turnClockwise original =
    original
        |> List.foldl
            (\row -> concat (row |> List.map List.singleton))
            []



-- Concatenate two grids


concat : Grid -> Grid -> Grid
concat a b =
    concatRec [] a b


concatRec : Grid -> Grid -> Grid -> Grid
concatRec acc a b =
    case ( a, b ) of
        ( aFirstRow :: aOthers, bFirstRow :: bOthers ) ->
            concatRec (acc ++ [ aFirstRow ++ bFirstRow ]) aOthers bOthers

        ( [], [] ) ->
            acc

        ( [], it ) ->
            it

        ( it, [] ) ->
            it



--


type alias RandomTile =
    { face : Int, coordinates : Coordinates }


randomTileGenerator : Grid -> Random.Generator (Maybe RandomTile)
randomTileGenerator grid =
    pickEmptySquare grid
        |> Random.pair randomFace
        |> Random.map (\( face, maybeCoord ) -> maybeCoord |> Maybe.map (RandomTile face))


pickEmptySquare : Grid -> Random.Generator (Maybe Coordinates)
pickEmptySquare grid =
    grid
        |> listAvailableSquares
        |> Random.List.choose
        |> Random.map Tuple.first


randomFace : Random.Generator Int
randomFace =
    Random.weighted ( 3, 2 ) [ ( 1, 4 ) ]


type alias Coordinates =
    { x : Int, y : Int }


listAvailableSquares : Grid -> List Coordinates
listAvailableSquares grid =
    grid
        |> List.indexedMap Tuple.pair
        |> List.map
            (\( y, row ) ->
                row
                    |> List.indexedMap Tuple.pair
                    |> List.filter (Tuple.second >> (==) 0)
                    |> List.map (\( x, _ ) -> { x = x, y = List.length grid - y })
            )
        |> List.foldr (++) []
