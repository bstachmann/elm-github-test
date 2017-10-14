module Main exposing (main)


import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)


import DagRenderer exposing (..)

type Msg
    = Nothing

sample_sections : List Section
sample_sections =
    [ { id = "A", x0 = 100, lane = lane 0, predecessor_lanes = [ lane 0, lane 1 ] }
    , { id = "B", x0 = 100, lane = lane 1, predecessor_lanes = [ lane 1 ] }
    , { id = "C", x0 = 100, lane = lane 2, predecessor_lanes = [ lane 2, lane 0 ] }
    , { id = "D", x0 = 100, lane = lane 3, predecessor_lanes = [] }
    , { id = "E", x0 = 100, lane = lane 4, predecessor_lanes = [ lane 4, lane 1 ] }
    ]


main : Html Msg
main =
    Html.body []
        [ Html.text "Hello, World!"
        , Html.br [] []
        , svg
            [ version "1.1", x "0", y "0", viewBox "0 0 640px 320px" ]
          <|
            List.concatMap render sample_sections
        ]
