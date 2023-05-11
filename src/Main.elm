port module Main exposing (Model, Msg, main)

import Browser
import Color
import Color.Convert
import Color.Interpolate
import Css
import Grid
import Hsv
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events as Evts
import List.Extra
import Move
import Random


port swipe : (String -> msg) -> Sub msg


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> Html.toUnstyled
        , update = update
        , subscriptions = always (swipe Swipe)
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
    | Swipe String


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

        Swipe direction ->
            let
                maybeMove =
                    case direction of
                        "left" ->
                            Just Move.Left

                        "right" ->
                            Just Move.Right

                        "up" ->
                            Just Move.Up

                        "down" ->
                            Just Move.Down

                        _ ->
                            Nothing
            in
            case maybeMove of
                Just move ->
                    Grid.handle move model
                        |> (tuplize >> Tuple.mapSecond addRandomTile)

                Nothing ->
                    ( model, Cmd.none )


tuplize : a -> ( a, a )
tuplize a =
    ( a, a )



-- View


view : Model -> Html Msg
view grid =
    Html.div
        [ css
            [ Css.backgroundColor <| Css.hex "#eee"
            , Css.position Css.absolute
            , Css.top Css.zero
            , Css.bottom Css.zero
            , Css.left Css.zero
            , Css.right Css.zero
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
                    , Css.border3 (Css.px 1) Css.solid (Css.hex "#999")
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
                                            Html.div
                                                [ css
                                                    [ Css.width <| Css.vmin 16
                                                    , Css.height <| Css.vmin 16
                                                    , Css.textAlign Css.center
                                                    , Css.fontSize <| Css.vmin 5
                                                    , Css.padding <| Css.vmin 1
                                                    ]
                                                ]
                                                [ viewTile tile
                                                ]
                                        )
                                )
                        )
                )
            , Html.div
                [ css [ Css.displayFlex ]
                ]
                [ Html.button
                    [ Evts.onClick <| Moved Move.Up
                    , css [ Css.flexGrow <| Css.num 1 ]
                    ]
                    [ Html.text "Up"
                    ]
                , Html.button
                    [ Evts.onClick <| Moved Move.Left
                    , css [ Css.flexGrow <| Css.num 1 ]
                    ]
                    [ Html.text "Left"
                    ]
                , Html.button
                    [ Evts.onClick <| Moved Move.Right
                    , css [ Css.flexGrow <| Css.num 1 ]
                    ]
                    [ Html.text "Right"
                    ]
                , Html.button
                    [ Evts.onClick <| Moved Move.Down
                    , css [ Css.flexGrow <| Css.num 1 ]
                    ]
                    [ Html.text "Down"
                    ]
                ]
            ]
        ]


viewTile : Int -> Html Msg
viewTile tile =
    Html.div
        [ css
            [ Css.height <| Css.pct 100
            , Css.width <| Css.pct 100
            , Css.borderRadius <| Css.vmin 2
            , Css.displayFlex
            , Css.alignItems Css.center
            , Css.justifyContent Css.center
            , Css.border3 (Css.px 1) Css.solid (Css.hex "#999")
            , Css.property "background-color" <|
                if tile == 0 then
                    Color.Convert.hexToColor "#eee"
                        |> Result.withDefault Color.black
                        |> Color.Convert.colorToCssHsl

                else
                    let
                        tileRadical =
                            tile |> toFloat |> logBase 2

                        targetRadical =
                            2048 |> logBase 2

                        coef =
                            tileRadical / targetRadical
                    in
                    { hue = 360 * coef |> floor
                    , saturation = 1
                    , value = 1
                    }
                        |> Hsv.toCssString
            ]
        ]
        [ Html.div []
            [ Html.text <|
                String.replace "0" "" <|
                    String.fromInt tile
            ]
        ]



-- 2^x = 2048
