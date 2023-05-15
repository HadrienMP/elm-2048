module Routing exposing (..)

import AppUrl exposing (AppUrl)
import Url


type Route
    = Game
    | Palette


parseUrl : Url.Url -> Route
parseUrl =
    AppUrl.fromUrl >> parse >> Maybe.withDefault Game


parse : AppUrl -> Maybe Route
parse url =
    case url.path of
        [] ->
            Just Game

        [ "palette" ] ->
            Just Palette

        _ ->
            Nothing


toUrl : Route -> AppUrl
toUrl page =
    case page of
        Game ->
            AppUrl.fromPath []

        Palette ->
            AppUrl.fromPath [ "palette" ]
