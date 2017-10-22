module DagRenderer exposing (..)

import List exposing (append, concatMap, map, drop, head, foldl, range)
import Array exposing (Array)
import Maybe
import Svg exposing (..)
import Svg.Attributes exposing (..)


type Cell i = NewCell i (Array Int)


type StreamLayout i = NewStreamLayout Int Int (Array (Cell i))


type alias ColumnId = Int


type alias LaneId = Int


empty : Int -> StreamLayout i
empty nrOfLanes =
    let
        nrOfColumns = 0
    in
        NewStreamLayout nrOfLanes nrOfColumns Array.empty

appendColumn :  StreamLayout i  -> StreamLayout i
appendColumn (NewStreamLayout nrOfLanes nrOfColumns data) =
    NewStreamLayout nrOfLanes (nrOfColumns + 1) data


newRender : StreamLayout i  -> List (Svg m)
newRender (NewStreamLayout nrOfLanes nrOfColumns data as layout) =
    range 0 (nrOfColumns - 1)
    |> List.foldl (renderColumn layout) []


renderColumn : StreamLayout i -> ColumnId -> List (Svg m) -> List (Svg m)
renderColumn (NewStreamLayout nrOfLanes nrOfColumns data) column acc =
  let
    columnWidth = 120
    connectorWidth = 40
    sectionWidth = columnWidth - connectorWidth

    rowHeight = 40
    laneHeight = 30

    columnHeight = rowHeight * nrOfLanes

    x0 = columnWidth * column
    y0 = 0
  in
    rect
      ( [fill "pink" ] |> inBox x0 y0 sectionWidth columnHeight )
      []
    :: acc


inBox : Int -> Int -> Int -> Int -> List (Attribute m) -> List (Attribute m)
inBox x0 y0 w h acc =
    x (toString x0)
    :: y (toString y0)
    :: width (toString w)
    :: height (toString h)
    :: acc


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


connectorWidth =
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
    , x0 : Int
    , lane : Lane
    , predecessor_lanes : List Lane
    }


point : Int -> Int -> String
point x y =
  (toString x) ++ "," ++ (toString y) ++ " "

renderConnection : Section -> Lane -> List (Svg m)
renderConnection section pred =
    [ polyline
        [ fill pred.color
        , fillOpacity "0.7"
        , points
            ( (point (section.x0 - connectorWidth) pred.y0)
            ++ (point section.x0 section.lane.y0)
            ++ (point section.x0 section.lane.y1)
            ++ (point (section.x0 - connectorWidth) pred.y1)
            )
        ]
        []
    ]

render : Section -> List (Svg m)
render section =
    rect [ x (toString section.x0), y (toString section.lane.y0), width (toString sectionWidth), height (toString laneHeight), fill section.lane.color, fillOpacity "0.7" ] []
        :: text_ [ x (toString section.x0), y (toString section.lane.y1) ] [ text <| "<" ++ section.id ++ ">" ]
        :: (concatMap (renderConnection section) <| List.reverse <| section.predecessor_lanes)
