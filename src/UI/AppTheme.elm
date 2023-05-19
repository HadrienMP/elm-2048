module UI.AppTheme exposing (..)

import Color
import Color.Convert
import Css


type alias Colors =
    { shadow : Color.Color
    , surface : Color.Color
    , background : Color.Color
    }


type alias Theme =
    { solid : Colors
    , on : Colors
    }


default : Theme
default =
    { solid =
        { shadow = hex "ccc"
        , surface = hex "ddd"
        , background = Color.white
        }
    , on =
        { shadow = Color.red
        , surface = Color.white
        , background = Color.black
        }
    }


hex : String -> Color.Color
hex =
    Color.Convert.hexToColor >> Result.withDefault Color.purple


toCss : Color.Color -> Css.Color
toCss =
    Color.toRgba
        >> (\{ red, green, blue, alpha } ->
                Css.rgba (red * 255 |> round)
                    (green * 255 |> round)
                    (blue * 255 |> round)
                    alpha
           )
