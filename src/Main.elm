port module Main exposing (Model, Msg, main)

import Browser
import Browser.Navigation
import Html.Styled as Html exposing (Html)
import Pages.Game
import Pages.Palette
import Routing
import Shared
import Url


port swipe : (String -> msg) -> Sub msg


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = Shared.UrlChanged >> SharedMsg
        , onUrlRequest = Shared.LinkClicked >> SharedMsg
        }



-- Init


type Page
    = Palette
    | Game Pages.Game.Model


type alias Model =
    { shared : Shared.Model
    , page : Page
    }


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( shared, sharedCmd ) =
            Shared.init { url = url, key = key }

        ( page, pageCmd ) =
            case shared.route of
                Routing.Palette ->
                    ( Palette, Cmd.none )

                Routing.Game ->
                    Pages.Game.init ()
                        |> Tuple.mapBoth
                            Game
                            (Cmd.map GameMsg)
    in
    ( { shared = shared
      , page = page
      }
    , Cmd.batch
        [ Cmd.map SharedMsg sharedCmd
        , Cmd.map GotPageMsg pageCmd
        ]
    )



-- Update


type Msg
    = SharedMsg Shared.Msg
    | GotPageMsg PageMsg


type PageMsg
    = GameMsg Pages.Game.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SharedMsg subMsg ->
            Shared.update subMsg model.shared
                |> Tuple.mapBoth
                    (\next -> { model | shared = next })
                    (Cmd.map SharedMsg)

        GotPageMsg pageMsg ->
            let
                ( page, pageCmd ) =
                    case ( pageMsg, model.page ) of
                        ( GameMsg subMsg, Game subModel ) ->
                            Pages.Game.update subMsg subModel
                                |> Tuple.mapBoth
                                    Game
                                    (Cmd.map GameMsg)

                        _ ->
                            ( model.page, Cmd.none )
            in
            ( { model | page = page }, Cmd.map GotPageMsg pageCmd )



-- Subscription


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Game game ->
            Pages.Game.subscriptions game |> Sub.map (GotPageMsg << GameMsg)

        _ ->
            Sub.none



-- View


view : Model -> Browser.Document Msg
view model =
    { title = "2048"
    , body =
        [ viewPage model.page |> Html.map GotPageMsg |> Html.toUnstyled ]
    }


viewPage : Page -> Html PageMsg
viewPage model =
    case model of
        Palette ->
            Pages.Palette.view ()

        Game game ->
            Pages.Game.view game |> Html.map GameMsg
