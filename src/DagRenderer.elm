module DagRenderer exposing (..)

import List exposing (append, concatMap, map, drop, head, foldl, range)
import Array exposing (Array)
import Set
import Maybe
import Dict exposing (Dict)
import Svg exposing (..)
import Svg.Attributes exposing (..)

{-- Types --}

type Cell i
    = NewCell i (List Int)


type StreamLayout i
    = NewStreamLayout Int Int (Dict Int (Cell i))


type alias ColumnId =
    Int


type alias LaneId =
    Int


{-- Building layouts --}


empty : Int -> StreamLayout i
empty nrOfLanes =
    let
        nrOfColumns =
            0
    in
        NewStreamLayout nrOfLanes nrOfColumns Dict.empty


appendColumn : StreamLayout i -> StreamLayout i
appendColumn (NewStreamLayout nrOfLanes nrOfColumns data) =
    NewStreamLayout nrOfLanes (nrOfColumns + 1) data


appendCell : LaneId -> i -> List Int -> StreamLayout i -> StreamLayout i
appendCell lane i successors (NewStreamLayout nrOfLanes nrOfColumns data) =
    let
        lastColumn =
            nrOfColumns - 1

        nextData =
            Dict.insert (lane + nrOfLanes * lastColumn) (NewCell i successors) data
    in
        NewStreamLayout nrOfLanes nrOfColumns nextData

{-- Rendering to SVG --}

config =
    { columnWidth = 120
    , connectorWidth = 40
    , rowHeight = 50
    , laneHeight = 30
    , diagnosticsFor =
        Set.fromList
            [ "debug"

            -- , "column"
            -- , "section"
            -- , "cell"
            ]

    , laneColors =
          [ "#FFBBBB"
          , "#BBFFBB"
          , "#BBBBFF"
          , "#BBFFFF"
          , "#FFFFBB"
          , "#FFBBFF"
          ]
          |> Array.fromList
    }


newRender : StreamLayout i -> List (Svg m)
newRender ((NewStreamLayout nrOfLanes nrOfColumns data) as layout) =
    range 0 (nrOfColumns - 1)
        |> List.foldl (renderColumn layout) []


renderColumn : StreamLayout i -> ColumnId -> List (Svg m) -> List (Svg m)
renderColumn ((NewStreamLayout nrOfLanes nrOfColumns data) as layout) column acc =
    let
        x0 =
            config.columnWidth * column
    in
        acc
            |> diagnostic "column" column "green" ( x0, 0, config.columnWidth, nrOfLanes * config.rowHeight )
            |> (\acc -> foldl (renderSection layout column) acc (range 0 (nrOfLanes - 1)))



renderSection : StreamLayout i -> ColumnId -> LaneId -> List (Svg m) -> List (Svg m)
renderSection ((NewStreamLayout nrOfLanes nrOfColumns data) as layout) column lane acc =
    let
        x_ =
            column * config.columnWidth

        y_ =
            0 + (config.rowHeight * lane)

        index =
            column * nrOfLanes + lane

        m =
            Dict.get index data
    in
        m
            |> Maybe.map (\c -> renderCell layout x_ y_ c acc)
            |> Maybe.withDefault acc
            |> diagnostic "section" lane "blue" ( x_, y_, config.columnWidth, config.laneHeight )


renderCell : StreamLayout i -> Int -> Int -> Cell i -> List (Svg m) -> List (Svg m)
renderCell ((NewStreamLayout nrOfLanes nrOfColumns data) as layout) section_x section_y (NewCell i successors) acc =
    let
        bounds =
            ( section_x, section_y, config.columnWidth - config.connectorWidth, config.laneHeight )
    in
        rect ([ fill <| colorForLane <| section_y // config.rowHeight, fillOpacity "0.6" ] |> inBox bounds) []
            :: text_ [ x (toString (section_x + 4)), y (toString (section_y + 14)), fill "blue" ] [ text <| toString i ]
            :: acc
            |> (\acc -> List.foldl (newRenderConnections layout section_x section_y) acc successors)
            |> diagnostic "cell" lane "red" bounds


newRenderConnections : StreamLayout i -> Int -> Int -> LaneId -> List (Svg m) -> List (Svg m)
newRenderConnections ((NewStreamLayout nrOfLanes nrOfColumns data) as layout) section_x section_y successorLane acc =
    let
        xRight = section_x + config.columnWidth
        xLeft = xRight - config.connectorWidth

        xA = xRight + ((config.connectorWidth * 1) // 4)
        xB = xRight + ((config.connectorWidth * 2) // 4)
        xC = xRight + ((config.connectorWidth * 3) // 4)

        yLeftTop = section_y
        yLeftBottom = yLeftTop + config.laneHeight

        yMiddleTop  =  (yLeftTop + yRightTop) // 2
        yMiddleBottom  =  (yLeftBottom + yRightBottom) // 2

        yRightTop = successorLane * config.rowHeight
        yRightBottom = yRightTop + config.laneHeight
    in
        polyline
            [ fill (colorForLane successorLane)
            , fillOpacity "0.6"
            , points
                ((point xLeft yLeftTop)
                -- Move right
                ++ (point xA yLeftTop)
                ++ (point xB yMiddleTop)
                ++ (point xC yRightTop)
                ++ (point xRight yRightTop)
                -- Move down
                ++ (point xRight yRightBottom)
                -- Move left
                ++ (point xC yRightBottom)
                ++ (point xB yMiddleBottom)
                ++ (point xA yLeftBottom)
                ++ (point xLeft yLeftBottom)
                )
            ]
            []
            :: acc

{-- Implementation helpers --}


colorForLane : LaneId -> String
colorForLane lane =
    Array.get (lane % (Array.length config.laneColors)) config.laneColors |> Maybe.withDefault "blue"


diagnostic : String -> e -> String -> ( Int, Int, Int, Int ) -> List (Svg m) -> List (Svg m)
diagnostic t msg color_ (( x0, y0, _, _ ) as bounds) acc =
    if Set.member t config.diagnosticsFor then
        rect ([ stroke color_, strokeOpacity "0.5", fill "none" ] |> inBox bounds) []
            :: text_ [ x (toString (x0 + 2)), y (toString (y0 + 12)), fill color_, fillOpacity "0.5" ] [ text (toString msg) ]
            :: acc
    else
        acc


inBox : ( Int, Int, Int, Int ) -> List (Attribute m) -> List (Attribute m)
inBox ( x0, y0, w, h ) acc =
    x (toString x0)
        :: y (toString y0)
        :: width (toString w)
        :: height (toString h)
        :: acc


{-- Deprecated Stuff --}

laneColors =
        [ "#FFBBBB"
        , "#BBFFBB"
        , "#BBBBFF"
        , "#BBFFFF"
        , "#FFFFBB"
        , "#FFBBFF"
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
            ((point (section.x0 - connectorWidth) pred.y0)
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
