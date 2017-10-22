module DagRenderer exposing (..)

import List exposing (append, concatMap, map, drop, head, foldl, range)
import Array exposing (Array)
import Set
import Maybe
import Svg exposing (..)
import Svg.Attributes exposing (..)


type Cell i = NewCell i (Array Int)


type StreamLayout i = NewStreamLayout Int Int (Array (Cell i))


type alias ColumnId = Int


type alias LaneId = Int

config =
  { columnWidth = 120
    , connectorWidth = 40
    , rowHeight = 50
    , laneHeight = 30
    , diagnosticsFor =
        Set.fromList
            [ "debug"
            --, "column"
            --, "section"
            , "cell"
            ]
  }


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
renderColumn (NewStreamLayout nrOfLanes nrOfColumns data as layout) column acc =
  let
    x0 = config.columnWidth * column
  in
    foldl (renderSection
   layout x0) acc (range 0 (nrOfLanes - 1))
    |> diagnostic "column" "green" (x0, 0, config.columnWidth, nrOfLanes * config.rowHeight)

diagnostic : String -> String -> (Int, Int, Int, Int) -> List (Svg m) -> List (Svg m)
diagnostic a color_ ((x0, y0, _, _) as bounds) acc =
  if Set.member a config.diagnosticsFor then
      rect ( [stroke color_, strokeOpacity "0.5",  fill "none" ] |> inBox bounds ) []
      :: text_ [ x (toString (x0 + 2)), y (toString (y0 + 12)), fill color_ , fillOpacity "0.5"] [ text a ]
      :: acc
  else
      acc

inBox : (Int, Int, Int, Int) -> List (Attribute m) -> List (Attribute m)
inBox (x0, y0, w, h) acc =
    x (toString x0)
    :: y (toString y0)
    :: width (toString w)
    :: height (toString h)
    :: acc

renderSection : StreamLayout i -> Int -> LaneId -> List (Svg m) -> List (Svg m)
renderSection (NewStreamLayout nrOfLanes nrOfColumns data as layout) column_x lane acc =
  let
    x_ = column_x
    y_ = 0 + (config.rowHeight * lane)
  in
    acc
    |> renderCell layout x_ y_
    |> diagnostic "section" "blue" (x_, y_, config.columnWidth, config.laneHeight)


renderCell : StreamLayout i -> Int -> Int -> List (Svg m) -> List (Svg m)
renderCell (NewStreamLayout nrOfLanes nrOfColumns data as layout) section_x section_y acc =
    acc
    |> diagnostic "cell" "red" (section_x, section_y, config.columnWidth - config.connectorWidth, config.laneHeight)




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
