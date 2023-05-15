module Shared exposing (..)

import Browser
import Browser.Navigation
import Routing
import Url



-- Init


type alias Model =
    { key : Browser.Navigation.Key
    , route : Routing.Route
    }


init : { url : Url.Url, key : Browser.Navigation.Key } -> ( Model, Cmd Msg )
init { url, key } =
    ( { key = key, route = url |> Routing.parseUrl }
    , Cmd.none
    )



-- Update


type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Browser.Navigation.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Browser.Navigation.load href )

        UrlChanged url ->
            ( { model | route = url |> Routing.parseUrl }
            , Cmd.none
            )
