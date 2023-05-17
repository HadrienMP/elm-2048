module UI.AppTheme exposing (..)

import Color
import Color.Convert


type alias Colors =
    { shadow : Color.Color
    , surface : Color.Color
    , background : Color.Color
    }


type alias Theme =
    { below : Colors
    , on : Colors
    }


default : Theme
default =
    { below =
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
