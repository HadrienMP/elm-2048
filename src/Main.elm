port module Main exposing (Model, Msg, main, viewTile)

import Browser
import Color
import Color.Accessibility
import Css
import Grid
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
        |> (toPair >> Tuple.mapSecond addRandomTile)


addRandomTile : Grid.Grid -> Cmd Msg
addRandomTile grid =
    Grid.randomTileGenerator grid
        |> Random.generate Updated



-- Update


type Msg
    = Updated (Maybe Grid.RandomTile)
    | Moved Move.Move
    | Swipe String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Updated maybeTile ->
            case maybeTile of
                Just tile ->
                    ( model
                        |> List.Extra.updateAt tile.coordinates.y
                            (List.Extra.updateAt tile.coordinates.x
                                (always tile.face)
                            )
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        Moved move ->
            Grid.handle move model
                |> toPair
                |> Tuple.mapSecond addRandomTile

        Swipe direction ->
            case Move.parse direction of
                Just move ->
                    let
                        next =
                            Grid.handle move model
                    in
                    ( next
                    , if next == model then
                        Cmd.none

                      else
                        addRandomTile next
                    )

                Nothing ->
                    ( model, Cmd.none )


toPair : a -> ( a, a )
toPair a =
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
                                            Html.div [ css [ Css.padding (Css.vmin 0.6) ] ]
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


viewTile : Int -> Html msg
viewTile tile =
    let
        backgroundColor =
            if tile == 0 then
                Color.white

            else
                let
                    tileRadical =
                        tile
                            |> toFloat
                            |> logBase 2

                    targetRadical =
                        2048
                            |> logBase 2
                in
                Color.fromHsla
                    { hue = tileRadical / targetRadical
                    , saturation = 1
                    , lightness = 0.7
                    , alpha = 1
                    }

        textColor =
            Color.Accessibility.maximumContrast backgroundColor [ Color.white, Color.black ]
                |> Maybe.withDefault Color.black
    in
    Html.div
        [ css
            [ Css.height <| Css.pct 100
            , Css.width <| Css.pct 100
            , Css.borderRadius <| Css.vmin 2
            , Css.displayFlex
            , Css.alignItems Css.center
            , Css.justifyContent Css.center
            , Css.border3 (Css.px 1) Css.solid (Css.hex "#999")
            , Css.backgroundColor <| toCssColor backgroundColor
            , Css.color <|
                toCssColor textColor
            , Css.width <| Css.vmin 16
            , Css.height <| Css.vmin 16
            , Css.textAlign Css.center
            , Css.fontSize <| Css.vmin 5
            ]
        ]
        [ Html.div []
            [ Html.text <|
                case tile of
                    0 ->
                        ""

                    _ ->
                        String.fromInt tile
            ]
        ]


toCssColor : Color.Color -> Css.Color
toCssColor =
    Color.toRgba
        >> (\it -> Css.rgba (it.red * 255 |> round) (it.green * 255 |> round) (it.blue * 255 |> round) it.alpha)
