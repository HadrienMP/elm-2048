module Move exposing (Move(..), parse)


type Move
    = Left
    | Right
    | Down
    | Up


parse : String -> Maybe Move
parse raw =
    case raw of
        "left" ->
            Just Left

        "right" ->
            Just Right

        "up" ->
            Just Up

        "down" ->
            Just Down

        _ ->
            Nothing
