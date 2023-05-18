port module Pages.Game exposing (Model, Msg, init, main, subscriptions, update, view)

import Browser
import Game.Grid as Grid
import Game.Move as Move
import Html.Styled as Html exposing (Html)
import List.Extra
import Random


port swipe : (String -> msg) -> Sub msg


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> Html.toUnstyled
        , update = update
        , subscriptions = subscriptions
        }



-- Init


type alias Model =
    Grid.Grid


init : () -> ( Model, Cmd Msg )
init _ =
    let
        grid =
            Grid.init
    in
    ( grid, addRandomTile 2 grid )


addRandomTile : Int -> Grid.Grid -> Cmd Msg
addRandomTile face grid =
    Grid.randomTileGenerator face grid
        |> Random.generate Updated



-- Update


type Msg
    = Updated (List Grid.RandomTile)
    | Moved Move.Move
    | Swipe String


insert : List Grid.RandomTile -> Grid.Grid -> Grid.Grid
insert tiles grid =
    case tiles of
        [] ->
            grid

        tile :: tail ->
            insert tail
                (List.Extra.updateAt tile.coordinates.y
                    (List.Extra.updateAt tile.coordinates.x
                        (always tile.face)
                    )
                    grid
                )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Updated tiles ->
            ( insert tiles model
            , Cmd.none
            )

        Moved move ->
            Grid.handle move model
                |> toPair
                |> Tuple.mapSecond (addRandomTile 1)

        Swipe direction ->
            case Move.parse direction of
                Just move ->
                    let
                        next =
                            Grid.handle move model
                    in
                    ( next
                    , if next == model then
                        Cmd.none

                      else
                        addRandomTile 1 next
                    )

                Nothing ->
                    ( model, Cmd.none )


toPair : a -> ( a, a )
toPair a =
    ( a, a )



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions =
    always (swipe Swipe)



-- View


view : Grid.Grid -> Html Msg
view grid =
    Grid.view grid
