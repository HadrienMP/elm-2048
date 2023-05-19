module UI.Style exposing (..)

import Css


roundedCorners : Css.Style
roundedCorners =
    Css.borderRadius (Css.vmin 2)
