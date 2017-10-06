module Main exposing (main)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Gilm exposing (..)
import Html exposing (..)
import Maybe exposing (Maybe(Nothing))


main =
    Html.program
        { init = init Nothing
        , view = viewWithBootstrap
        , update = update
        , subscriptions = subscriptions
        }


viewWithBootstrap : Model -> Html Msg
viewWithBootstrap model =
    Grid.container []
        [ CDN.stylesheet
        , Gilm.view model
        ]
