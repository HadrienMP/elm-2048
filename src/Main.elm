module Main exposing (Model, Msg, main)

import Browser
import Css
import Grid
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events as Evts
import List.Extra
import Move
import Random


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> Html.toUnstyled
        , update = update
        , subscriptions = always Sub.none
        }



-- Init


type alias Model =
    Grid.Grid


init : () -> ( Model, Cmd Msg )
init _ =
    Grid.init
        |> (tuplize >> Tuple.mapSecond addRandomTile)


addRandomTile : Grid.Grid -> Cmd Msg
addRandomTile grid =
    Grid.randomTileGenerator grid
        |> Random.map
            (\tile ->
                grid
                    |> List.Extra.updateAt tile.coordinates.y
                        (List.Extra.updateAt tile.coordinates.x
                            (always tile.face)
                        )
            )
        |> Random.generate Updated



-- Update


type Msg
    = Updated Grid.Grid
    | Moved Move.Move


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Updated grid ->
            ( grid
            , Cmd.none
            )

        Moved move ->
            Grid.handle move model
                |> (tuplize >> Tuple.mapSecond addRandomTile)


tuplize : a -> ( a, a )
tuplize a =
    ( a, a )



-- View


view : Model -> Html Msg
view grid =
    Html.div [ css [ Css.displayFlex ] ]
        [ Html.table
            [ css
                [ Css.borderCollapse Css.collapse
                ]
            ]
            (grid
                |> List.map
                    (\row ->
                        Html.tr []
                            (row
                                |> List.map
                                    (\tile ->
                                        Html.td
                                            [ css
                                                [ Css.width <| Css.rem 4
                                                , Css.height <| Css.rem 4
                                                , Css.textAlign Css.center
                                                , Css.border3
                                                    (Css.px 1)
                                                    Css.solid
                                                    (Css.hex "#000")
                                                ]
                                            ]
                                            [ Html.text <|
                                                String.replace "0" "" <|
                                                    String.fromInt tile
                                            ]
                                    )
                            )
                    )
            )
        , Html.div
            [ css
                [ Css.position Css.relative
                , Css.alignSelf Css.center
                ]
            ]
            [ Html.button
                [ Evts.onClick <| Moved Move.Up
                , css
                    [ Css.display Css.block
                    , Css.width <| Css.px 60
                    , Css.padding <| Css.px 10
                    , Css.margin Css.auto
                    ]
                ]
                [ Html.text "Up"
                ]
            , Html.button
                [ Evts.onClick <| Moved Move.Left
                , css
                    [ Css.display Css.inlineBlock
                    , Css.width <| Css.px 60
                    , Css.padding <| Css.px 10
                    ]
                ]
                [ Html.text "Left"
                ]
            , Html.button
                [ Evts.onClick <| Moved Move.Right
                , css
                    [ Css.display Css.inlineBlock
                    , Css.width <| Css.px 60
                    , Css.padding <| Css.px 10
                    ]
                ]
                [ Html.text "Right"
                ]
            , Html.button
                [ Evts.onClick <| Moved Move.Down
                , css
                    [ Css.display Css.block
                    , Css.width <| Css.px 60
                    , Css.padding <| Css.px 10
                    , Css.margin Css.auto
                    ]
                ]
                [ Html.text "Down"
                ]
            ]
        ]
