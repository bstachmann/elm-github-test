module Main exposing (main)


import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)


import DagRenderer exposing (..)

type Msg
    = Nothing

sample : DagRenderer.StreamLayout String
sample =
    empty 7
    |> appendColumn
    |> appendColumn
    |> appendColumn
    |> appendColumn

  


main : Html Msg
main =
    Html.body []
        [ Html.text "Hello Stream Graph!"
        , Html.br [] []
        , svg
            [ version "1.1", x "0", y "0", width "1280px", height "1024px", viewBox "0 0 1280px 1024px" ]
          <|
            DagRenderer.newRender sample
        ]
