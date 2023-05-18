module Game.Grid exposing (Coordinates, Grid, RandomTile, handle, init, listAvailableSquares, randomTileGenerator, turnClockwise, turnCounterClockwise, view)

import Css
import Extra.Random
import Game.Move as Move exposing (Move)
import Game.Row as Row exposing (Row)
import Game.Tile as Tile
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Random


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
    { coordinates : Coordinates, face : Int }


type alias Coordinates =
    { x : Int, y : Int }


randomTileGenerator : Int -> Grid -> Random.Generator (List RandomTile)
randomTileGenerator number grid =
    listAvailableSquares grid
        |> Extra.Random.chooseN number
        |> Extra.Random.traverse affectRandomFace


listAvailableSquares : Grid -> List Coordinates
listAvailableSquares grid =
    grid
        |> List.indexedMap Tuple.pair
        |> List.map
            (\( y, row ) ->
                row
                    |> List.indexedMap Tuple.pair
                    |> List.filter (Tuple.second >> (==) 0)
                    |> List.map (\( x, _ ) -> { x = x, y = y })
            )
        |> List.foldr (++) []


affectRandomFace : Coordinates -> Random.Generator RandomTile
affectRandomFace coordinates =
    Random.weighted ( 3, 2 ) [ ( 1, 4 ) ]
        |> Random.map (\face -> { face = face, coordinates = coordinates })



-- View


view : Grid -> Html msg
view grid =
    Html.div
        [ css
            [ Css.borderRadius (Css.vmin 2)
            , Css.displayFlex
            , Css.flexDirection Css.column
            , Css.property "gap" "1vmin"
            , Css.maxWidth Css.fitContent
            , Css.margin Css.auto
            , Css.backgroundColor <| Css.hex "#ddd"
            , Css.padding2 (Css.vmin 1.6) (Css.vmin 2)
            , Css.boxShadow4 Css.zero (Css.vmin 1.2) Css.zero (Css.hex "#ccc")
            ]
        ]
        (grid
            |> List.map
                (\row ->
                    Html.div
                        [ css
                            [ Css.displayFlex
                            , Css.property "gap" "1.6vmin"
                            ]
                        ]
                        (row |> List.map Tile.view)
                )
        )
