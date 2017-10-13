module Main exposing (main)

import Html exposing (Html)
import List exposing (append, concatMap, map)
import Svg exposing (..)
import Svg.Attributes exposing (..)


type Msg = Nothing


laneColors =
    [ "#FFCCCC"
    , "#CCFFCC"
    , "#CCCCFF"
    , "#CCFFFF"
    , "#FFFFCC"
    , "#FFCCFF"
    ]


type alias Lane =
    { nr : Int
    }


lane : Int -> Lane
lane nr = { nr = nr }


type Section
    = Section Lane String (List Lane)


sample_sections : List Section
sample_sections =
    [ Section (lane 0) "A" [ lane 0, lane 2 ]
    , Section (lane 1) "B" [ lane 1 ]
    , Section (lane 2) "C" [ lane 0, lane 2 ]
    , Section (lane 3) "D" [ lane 3, lane 2 ]
    ]


render : Section -> List (Svg Msg)
render section =
    case section of
        Section l id predecessor_lanes ->
            let
                y0 =
                    l.nr * 30

                y0_ =
                    toString y0

                y1 =
                    y0 + 25

                y1_ =
                    toString y1
            in
            [ rect [ x "50", y y0_, width "80", height "25", fill "#FFCCCC" ] []
            , text_ [ x "50", y y1_ ] [ text <| "moin!" ++ id ]
            ]


main : Html Msg
main =
    Html.body []
        [ Html.text "Hello, World!"
        , Html.br [] []
        , svg
            [ version "1.1", x "0", y "0", viewBox "0 0 323.141 322.95" ]
          <|
            concatMap render sample_sections
        ]
