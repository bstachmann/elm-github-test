module Main exposing (main)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid

import Html exposing (..)

import Gilm exposing (..)


main =
    Html.program
        { init = init "rpreissel"
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
