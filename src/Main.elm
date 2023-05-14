port module Main exposing (Model, Msg, main)

import Browser
import Grid
import Html.Styled as Html exposing (Html)
import List.Extra
import Move
import Random


port swipe : (String -> msg) -> Sub msg


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> Html.toUnstyled
        , update = update
        , subscriptions = always (swipe Swipe)
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
addRandomTile number grid =
    Grid.randomTileGenerator number grid
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
            insert tail (List.Extra.updateAt tile.coordinates.y (List.Extra.updateAt tile.coordinates.x (always tile.face)) grid)


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



-- View


view : Grid.Grid -> Html Msg
view grid =
    Grid.view grid
