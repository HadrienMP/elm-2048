module Pages.Palette exposing (view)

import Game.Grid as Grid
import Game.Tile
import Html.Styled as Html exposing (Html)
import List.Extra


view : () -> Html msg
view _ =
    List.range 1 12
        |> List.map (\exponent -> { value = 2 ^ exponent, event = Game.Tile.None })
        |> List.Extra.groupsOf 4
        |> Grid.view
