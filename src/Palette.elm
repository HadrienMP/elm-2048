module Palette exposing (Model, Msg, main)

import Browser
import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Main


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> Html.toUnstyled
        , update = update
        , subscriptions = always Sub.none
        }



-- Init


type alias Model =
    ()


init : () -> ( Model, Cmd Msg )
init _ =
    ( (), Cmd.none )



-- Update


type alias Msg =
    ()


update : Msg -> Model -> ( Model, Cmd Msg )
update _ _ =
    ( (), Cmd.none )



-- View


view : Model -> Html Msg
view _ =
    List.range 1 12
        |> List.map ((^) 2)
        |> List.map Main.viewTile
        |> Html.div
            [ Attr.css
                [ Css.displayFlex
                , Css.flexWrap Css.wrap
                ]
            ]
