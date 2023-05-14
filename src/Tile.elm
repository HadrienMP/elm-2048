module Tile exposing (..)

import Color
import Color.Accessibility
import Css
import Css.Extra
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)


view : Int -> Html msg
view tile =
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
            , Css.backgroundColor <| Css.Extra.toCssColor backgroundColor
            , Css.color <|
                Css.Extra.toCssColor textColor
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
