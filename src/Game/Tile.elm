module Game.Tile exposing (..)

import Color
import Color.Convert
import Color.Interpolate
import Css
import Css.Animations
import Css.Extra
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import UI.Style


type alias Tile =
    { value : Int
    , event : TileEvent
    }


type TileEvent
    = None
    | Appeared


normalTile : Int -> Tile
normalTile value =
    { value = value, event = None }


newTile : Int -> Tile
newTile value =
    { value = value, event = Appeared }


view : Tile -> Html msg
view tile =
    Html.div
        [ css <|
            [ Css.height <| Css.pct 100
            , Css.width <| Css.pct 100
            , UI.Style.roundedCorners
            , Css.displayFlex
            , Css.alignItems Css.center
            , Css.justifyContent Css.center
            , Css.backgroundColor <| Css.Extra.toCssColor <| backgroundColor tile
            , Css.color <| Css.hex "#fff"
            , Css.width <| Css.vmin 18
            , Css.height <| Css.vmin 18
            , Css.textAlign Css.center
            , Css.fontSize <| Css.vmin 8
            , Css.fontWeight Css.bold
            , Css.property
                "text-shadow"
                "0 -0.5vmin  0px rgba(0,0,0,0.2)"
            , case tile.value of
                0 ->
                    Css.property "box-shadow" "inset 0 1.2vmin 0 #ccc"

                _ ->
                    Css.property "box-shadow" "inset 0 0.7vmin 0.4vmin #bbb"
            ]
                ++ (case tile.event of
                        None ->
                            []

                        Appeared ->
                            [ Css.animationDuration <| Css.ms 100
                            , Css.animationName <|
                                Css.Animations.keyframes
                                    [ ( 0
                                      , [ Css.Animations.opacity Css.zero
                                        , Css.Animations.transform [ Css.scale 0 ]
                                        ]
                                      )
                                    , ( 100
                                      , [ Css.Animations.opacity <| Css.num 100
                                        , Css.Animations.transform [ Css.scale 1 ]
                                        ]
                                      )
                                    ]
                            ]
                   )
        ]
        [ Html.div []
            [ Html.text <|
                case tile.value of
                    0 ->
                        ""

                    _ ->
                        String.fromInt tile.value
            ]
        ]


backgroundColor : Tile -> Color.Color
backgroundColor { value } =
    if value == 0 then
        Color.white

    else
        let
            radical =
                value
                    |> toFloat
                    |> logBase 2
                    |> subtract 1
                    |> round

            stuff =
                radical |> modBy 3 |> toFloat |> (\a -> a / 3)
        in
        case radical // 3 of
            0 ->
                Color.Interpolate.interpolate Color.Interpolate.RGB
                    (Color.Convert.hexToColor "07c8f9" |> Result.withDefault Color.red)
                    (Color.Convert.hexToColor "0d41e1" |> Result.withDefault Color.red)
                    stuff

            1 ->
                Color.Interpolate.interpolate Color.Interpolate.RGB
                    (Color.Convert.hexToColor "f20089" |> Result.withDefault Color.red)
                    (Color.Convert.hexToColor "8900f2" |> Result.withDefault Color.red)
                    stuff

            2 ->
                Color.Interpolate.interpolate Color.Interpolate.RGB
                    (Color.Convert.hexToColor "ff8500" |> Result.withDefault Color.red)
                    (Color.Convert.hexToColor "ff0000" |> Result.withDefault Color.red)
                    stuff

            3 ->
                Color.Interpolate.interpolate Color.Interpolate.RGB
                    (Color.Convert.hexToColor "52b69a" |> Result.withDefault Color.red)
                    (Color.Convert.hexToColor "168aad" |> Result.withDefault Color.red)
                    stuff

            _ ->
                Color.black


subtract : number -> number -> number
subtract b a =
    a - b
