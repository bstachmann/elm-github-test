module Main exposing (main)

import Html exposing (Html)
import List exposing (append, concatMap, map, drop, head)
import Maybe
import Svg exposing (..)
import Svg.Attributes exposing (..)


type Msg
    = Nothing


laneColors =
    [ "#FFDDDD"
    , "#DDFFDD"
    , "#DDDDFF"
    , "#DDFFFF"
    , "#FFFFDD"
    , "#FFDDFF"
    ]


type alias Lane =
    { nr : Int
    , y0 : Int
    , y1 : Int
    , color : String
    }


laneHeight =
    20


laneSeparator =
    10


sectionWidth =
    80


lane : Int -> Lane
lane nr =
    { nr = nr
    , y0 = nr * (laneHeight + laneSeparator)
    , y1 = nr * (laneHeight + laneSeparator) + laneHeight
    , color = (drop (nr % 6) laneColors |> head |> Maybe.withDefault "#DDDDDD")
    }


type alias Section =
    { id : String
    , lane : Lane
    , predecessor_lanes : List Lane
    }


sample_sections : List Section
sample_sections =
    [ { id = "A", lane = lane 0, predecessor_lanes = [ lane 0, lane 1 ] }
    , { id = "B", lane = lane 1, predecessor_lanes = [ lane 1 ] }
    , { id = "C", lane = lane 2, predecessor_lanes = [ lane 0, lane 2 ] }
    , { id = "D", lane = lane 3, predecessor_lanes = [] }
    , { id = "E", lane = lane 4, predecessor_lanes = [ lane 4, lane 1 ] }
    ]


render : Section -> List (Svg Msg)
render section =
    rect [ x "50", y (toString section.lane.y0), width (toString sectionWidth), height (toString laneHeight), fill section.lane.color ] []
        :: text_ [ x "50", y (toString section.lane.y1) ] [ text <| "<" ++ section.id ++ ">" ]
        :: []


main : Html Msg
main =
    Html.body []
        [ Html.text "Hello, World!"
        , Html.br [] []
        , svg
            [ version "1.1", x "0", y "0", viewBox "0 0 640px 320px" ]
          <|
            concatMap render sample_sections
        ]
