module Row exposing (Row, moveLeft, moveRight)

import Html.Styled exposing (wbr)
import Tile exposing (Tile)


type alias Row =
    List Int


moveRight : Row -> Row
moveRight =
    List.reverse
        >> moveLeft
        >> List.reverse


moveLeft : Row -> Row
moveLeft row =
    recMoveLeft { zeros = 0, summed = [], toSum = row }


recMoveLeft : { zeros : Int, summed : List Int, toSum : List Int } -> Row
recMoveLeft { zeros, summed, toSum } =
    case toSum of
        0 :: tail ->
            recMoveLeft { zeros = zeros + 1, summed = summed, toSum = tail }

        first :: second :: tail ->
            if first == second then
                recMoveLeft { zeros = zeros + 1, summed = summed ++ [ first + second ], toSum = tail }

            else
                recMoveLeft { zeros = zeros, summed = summed ++ [ first ], toSum = second :: tail }

        _ ->
            summed ++ toSum ++ List.repeat zeros 0
