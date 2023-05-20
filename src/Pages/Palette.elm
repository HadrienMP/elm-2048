module Pages.Palette exposing (view)

import Game.Grid as Grid
import Game.Tile
import Html.Styled as Html exposing (Html)
import List.Extra


view : () -> Html msg
view _ =
    Html.div
        []
        [ Html.h1 [] [ Html.text "Palette" ]
        , viewPalete
        ]


viewPalete : Html msg
viewPalete =
    List.range 1 12
        |> List.map
            (\exponent ->
                { value =
                    if exponent == 0 then
                        0

                    else
                        2 ^ exponent
                , event = Game.Tile.None
                }
            )
        |> List.Extra.groupsOf 3
        |> Grid.view
