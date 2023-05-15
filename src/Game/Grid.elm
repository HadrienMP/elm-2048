module Game.Grid exposing (Coordinates, Grid, RandomTile, handle, init, listAvailableSquares, randomTileGenerator, turnClockwise, turnCounterClockwise, view)

import Css
import Game.Move as Move exposing (Move)
import Game.Row as Row exposing (Row)
import Game.Tile as Tile
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Random
import Random.List


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


randomTileGenerator : Int -> Grid -> Random.Generator (List RandomTile)
randomTileGenerator number grid =
    pickEmptySquares number grid
        |> Random.andThen
            (\tiles ->
                tiles
                    |> List.map (\coord -> randomFace |> Random.map (\face -> { face = face, coordinates = coord }))
                    |> List.foldr
                        (\generator acc ->
                            acc
                                |> Random.andThen
                                    (\randomTiles ->
                                        generator
                                            |> Random.map (\randomTile -> randomTile :: randomTiles)
                                    )
                        )
                        (Random.constant [])
            )


pickEmptySquares : Int -> Grid -> Random.Generator (List Coordinates)
pickEmptySquares number grid =
    grid
        |> listAvailableSquares
        |> Tuple.pair []
        |> Random.constant
        |> stuff number
        |> Random.map (Tuple.first >> List.filterMap identity)


stuff :
    Int
    -> Random.Generator ( List (Maybe Coordinates), List Coordinates )
    -> Random.Generator ( List (Maybe Coordinates), List Coordinates )
stuff number toto =
    if number == 0 then
        toto

    else
        toto
            |> Random.andThen
                (\( previousPicks, available ) ->
                    Random.List.choose available
                        |> Random.map (\( pick, nextAvailable ) -> ( pick :: previousPicks, nextAvailable ))
                )
            |> stuff (number - 1)


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
                    |> List.map (\( x, _ ) -> { x = x, y = y })
            )
        |> List.foldr (++) []



-- View


view : Grid -> Html msg
view grid =
    Html.div
        [ css
            [ Css.position Css.absolute
            , Css.top Css.zero
            , Css.bottom Css.zero
            , Css.left Css.zero
            , Css.right Css.zero
            , Css.fontFamily Css.sansSerif
            ]
        ]
        [ Html.div
            [ css
                [ Css.position Css.absolute
                , Css.top <| Css.pct 50
                , Css.left <| Css.pct 50
                , Css.transform <| Css.translate2 (Css.pct -50) (Css.pct -50)
                ]
            ]
            [ Html.div
                [ css
                    [ Css.borderRadius (Css.vmin 2)
                    , Css.display Css.block
                    , Css.maxWidth Css.fitContent
                    , Css.margin Css.auto
                    , Css.backgroundColor <| Css.hex "#ddd"
                    , Css.padding <| Css.vmin 0.5
                    ]
                ]
                (grid
                    |> List.map
                        (\row ->
                            Html.div
                                [ css
                                    [ Css.displayFlex
                                    ]
                                ]
                                (row
                                    |> List.map
                                        (\tile ->
                                            Html.div [ css [ Css.padding (Css.vmin 0.6) ] ]
                                                [ Tile.view tile
                                                ]
                                        )
                                )
                        )
                )
            ]
        ]
