module Css.Extra exposing (..)

import Color
import Css


toCssColor : Color.Color -> Css.Color
toCssColor =
    Color.toRgba
        >> (\it -> Css.rgba (it.red * 255 |> round) (it.green * 255 |> round) (it.blue * 255 |> round) it.alpha)
