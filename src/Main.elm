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
    let
        grid =
            Grid.init
    in
    ( grid, addRandomTile 2 grid )


addRandomTile : Int -> Grid.Grid -> Cmd Msg
addRandomTile number grid =
    Grid.randomTileGenerator number grid
        |> Random.generate Updated



-- Update


type Msg
    = Updated (List Grid.RandomTile)
    | Moved Move.Move
    | Swipe String


insert : List Grid.RandomTile -> Grid.Grid -> Grid.Grid
insert tiles grid =
    case tiles of
        [] ->
            grid

        tile :: tail ->
            insert tail (List.Extra.updateAt tile.coordinates.y (List.Extra.updateAt tile.coordinates.x (always tile.face)) grid)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Updated tiles ->
            ( insert tiles model
            , Cmd.none
            )

        Moved move ->
            Grid.handle move model
                |> toPair
                |> Tuple.mapSecond (addRandomTile 1)

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
                        addRandomTile 1 next
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
                                                [ viewTile tile
                                                ]
                                        )
                                )
                        )
                )
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
                |> Maybe.withDefault Color.white
    in
    Html.div
        [ css
            [ Css.height <| Css.pct 100
            , Css.width <| Css.pct 100
            , Css.borderRadius <| Css.vmin 2
            , Css.displayFlex
            , Css.alignItems Css.center
            , Css.justifyContent Css.center
            , Css.backgroundColor <| toCssColor backgroundColor
            , Css.color <|
                toCssColor textColor
            , Css.width <| Css.vmin 16
            , Css.height <| Css.vmin 16
            , Css.textAlign Css.center
            , Css.fontSize <| Css.vmin 6
            , Css.property "text-shadow" <|
                "0.16vmin 0 0 #fff"
                    ++ ", 0.16vmin 0.16vmin 0 #fff"
                    ++ ", 0 0.16vmin 0 #fff"
                    ++ ", -0.16vmin 0.16vmin 0 #fff"
                    ++ ", -0.16vmin 0 0 #fff"
                    ++ ", -0.16vmin -0.16vmin 0 #fff"
                    ++ ", 0 -0.16vmin 0 #fff"
                    ++ ", 0.16vmin -0.16vmin 0 #fff"
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
