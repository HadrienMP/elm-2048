module Game.Tile exposing (..)

import Color
import Color.Convert
import Color.Interpolate
import Css
import Css.Extra
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)


view : Int -> Html msg
view tile =
    Html.div
        [ css
            [ Css.height <| Css.pct 100
            , Css.width <| Css.pct 100
            , Css.borderRadius <| Css.vmin 2
            , Css.displayFlex
            , Css.alignItems Css.center
            , Css.justifyContent Css.center
            , Css.backgroundColor <| Css.Extra.toCssColor <| backgroundColor tile
            , Css.color <| Css.hex "#fff"
            , Css.width <| Css.vmin 20
            , Css.height <| Css.vmin 20
            , Css.textAlign Css.center
            , Css.fontSize <| Css.vmin 8
            , Css.fontWeight Css.bold
            , Css.property
                "text-shadow"
                "0 -0.3vmin  0px rgba(0,0,0,0.2)"
            , case tile of
                0 ->
                    Css.property "box-shadow" "inset 0 0.6vmin 0 #ccc"

                _ ->
                    Css.property "box-shadow" "inset 0 0.3vmin 0 #bbb"
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


backgroundColor : Int -> Color.Color
backgroundColor tile =
    if tile == 0 then
        Color.white

    else
        let
            tileRadical =
                tile
                    |> toFloat
                    |> logBase 2
                    |> subtract 1
                    |> round

            stuff =
                tileRadical |> modBy 3 |> toFloat |> (\a -> a / 3)
        in
        case tileRadical // 3 of
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
