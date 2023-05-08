module Move exposing (Move(..), parse)


type Move
    = Left
    | Right
    | Down
    | Up


parse : String -> Move
parse raw =
    case raw of
        "R" ->
            Right

        "D" ->
            Down

        "U" ->
            Up

        _ ->
            Left
