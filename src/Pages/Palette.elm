module Pages.Palette exposing (view)

import Grid
import Html.Styled as Html exposing (Html)
import List.Extra


view : () -> Html msg
view _ =
    List.range 1 12
        |> List.map ((^) 2)
        |> List.Extra.groupsOf 4
        |> Grid.view
