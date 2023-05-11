module Hsv exposing (..)

{-
   When
   0 ≤ H < 360 (0 is less than or equal to Hue and less than 360),
   0 ≤ S ≤ 1 (0 is less than or equal to saturation which is less than or equal to 1) and
   0 ≤ V ≤ 1 (0 is less than or equal to value and less than or equal to 1);

   C = V × S

   X = C × (1 - | (H / 60°) mod 2 - 1|)

   m = V - C

   (R, G, B) = ((R'+ m) ×255, (G'+ m) ×255, (B'+ m) ×255)
-}

import Color
import Css exposing (saturation)


toCssString :
    { hue : Int
    , saturation : Float
    , value : Float
    }
    -> String
toCssString { hue, saturation, value } =
    let
        c =
            value * saturation

        x1 : Float
        x1 =
            toFloat hue / 60 |> floor |> modBy 2 |> toFloat

        x2 : Float
        x2 =
            abs (x1 - 1)

        x =
            c * (1 - x2)

        m =
            value - c

        ( rp, gp, bp ) =
            case hue // 60 of
                0 ->
                    ( c, x, 0 )

                1 ->
                    ( x, c, 0 )

                2 ->
                    ( 0, c, x )

                3 ->
                    ( 0, x, c )

                4 ->
                    ( x, 0, c )

                5 ->
                    ( c, 0, x )

                _ ->
                    ( c, x, 0 )

        r =
            rp + m

        g =
            gp + m

        b =
            bp + m
    in
    Color.rgb r g b |> Color.toCssString
