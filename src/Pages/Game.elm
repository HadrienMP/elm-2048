port module Pages.Game exposing (Model, Msg, init, main, subscriptions, update, view)

import Browser
import Css
import Game.Grid as Grid
import Game.Move as Move
import Game.Tile as Tile
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import List.Extra
import Random
import UI.AppTheme
import UI.Style


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
    List (List Tile.Tile)


init : () -> ( Model, Cmd Msg )
init _ =
    let
        grid =
            Grid.init
                |> List.map (List.map Tile.normalTile)
    in
    ( grid
    , addRandomTile 2 grid
    )


addRandomTile : Int -> Model -> Cmd Msg
addRandomTile count grid =
    Grid.randomTileGenerator count (grid |> List.map (List.map .value))
        |> Random.generate RandomTiles



-- Update


type Msg
    = RandomTiles (List Grid.RandomTile)
    | Moved Move.Move
    | Swipe String


insert : List Grid.RandomTile -> Model -> Model
insert tiles grid =
    case tiles of
        [] ->
            grid

        tile :: tail ->
            insert tail
                (List.Extra.updateAt tile.coordinates.y
                    (List.Extra.updateAt tile.coordinates.x
                        (always <| Tile.newTile tile.face)
                    )
                    grid
                )


toGrid : Model -> Grid.Grid
toGrid =
    List.map (List.map .value)


fromGrid : Grid.Grid -> Model
fromGrid =
    List.map (List.map Tile.normalTile)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RandomTiles tiles ->
            ( insert tiles model
            , Cmd.none
            )

        Moved move ->
            Grid.handle move (toGrid model)
                |> fromGrid
                |> toPair
                |> Tuple.mapSecond (addRandomTile 1)

        Swipe direction ->
            case Move.parse direction of
                Just move ->
                    let
                        current =
                            model |> toGrid

                        next =
                            Grid.handle move current

                        nextModel =
                            next |> fromGrid
                    in
                    ( nextModel
                    , if next == current then
                        Cmd.none

                      else
                        addRandomTile 1 nextModel
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


view : Model -> Html Msg
view grid =
    Html.div
        [ Attr.css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            , Css.property "gap" "2vmin"
            ]
        ]
        [ Html.h1
            [ Attr.css
                [ Css.boxShadow4 Css.zero
                    (Css.vmin 1)
                    Css.zero
                    (UI.AppTheme.toCss UI.AppTheme.default.solid.shadow)
                , UI.Style.roundedCorners
                , Css.maxWidth Css.fitContent
                ]
            ]
            [ Tile.view { value = 2048, event = Tile.None }
            ]
        , grid |> Grid.view
        ]
