module DagRenderer exposing (..)

import List exposing (append, concatMap, map, drop, head, foldl, range)
import Color exposing (..)
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
    { columnWidth =120
    , connectorWidth = 80
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
          [ "#F8CCCC"
          , "#CCF8CC"
          , "#CCCCF8"
          , "#CCF8F8"
          , "#F8F8CC"
          , "#F8CCF8"
          ]
          |> Array.fromList
    , opacity = "0.7"
    }


newRender : StreamLayout i -> List (Svg m)
newRender ((NewStreamLayout nrOfLanes nrOfColumns data) as layout) =
    (range 0 (nrOfColumns - 1)
    |> List.foldl (renderColumn layout) []
    )


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
        index = column * nrOfLanes + lane
    in
          Dict.get index data
          |> Maybe.map (\c -> renderCell layout column lane c acc)
          |> Maybe.withDefault acc


renderCell : StreamLayout i -> Int -> Int -> Cell i -> List (Svg m) -> List (Svg m)
renderCell ((NewStreamLayout nrOfLanes nrOfColumns data) as layout) column lane (NewCell i successors) acc =
    let
        bounds =
            ( column * config.columnWidth, lane * config.rowHeight, config.columnWidth - config.connectorWidth, config.laneHeight )
    in
        rect ([ fill <| colorForLane lane, fillOpacity "1.0" ] |> inBox bounds) []
            :: text_
                [ x (toString (column * config.columnWidth + 4))
                , y (toString (lane * config.rowHeight + 14))
                , fill "blue"
                ]
                [ text <| toString i ]
            :: acc
            |> (\acc -> List.foldl (newRenderConnections layout column lane) acc successors)
            |> diagnostic "cell" lane "red" bounds


newRenderConnections : StreamLayout i -> Int -> Int -> LaneId -> List (Svg m) -> List (Svg m)
newRenderConnections ((NewStreamLayout nrOfLanes nrOfColumns data) as layout) column laneLeft laneRight acc =
    let
        xRight = (column + 1) * config.columnWidth
        xLeft = xRight - config.connectorWidth

        xA = xLeft + ((config.connectorWidth * 1) // 4)
        xB = xLeft + ((config.connectorWidth * 2) // 4)
        xC = xLeft + ((config.connectorWidth * 3) // 4)

        yLeftTop = laneLeft * config.rowHeight
        yLeftBottom = yLeftTop + config.laneHeight

        yRightTop = laneRight * config.rowHeight
        yRightBottom = yRightTop + config.laneHeight

        yMiddleTop  =  (yLeftTop + yRightTop) // 2
        yMiddleBottom  =  (yLeftBottom + yRightBottom) // 2

        gradientId = "gr" ++ (toString laneLeft) ++ "_" ++ (toString laneRight)
    in
        defGradient gradientId (colorForLane laneLeft) (colorForLane laneRight)
        :: Svg.path
            [ fill <| "url(#" ++ gradientId ++ ")"
            , d
                ( -- Move right
                  "M "
                ++ (point xLeft yLeftTop)
                ++ "Q "
                ++ (point xA yLeftTop)
                ++ (point xB yMiddleTop)
                ++ "Q "
                ++ (point xC yRightTop)
                ++ (point xRight yRightTop)
                -- Move down
                ++ "L "
                ++ (point xRight yRightBottom)
                -- Move left
                ++ "Q "
                ++ (point xC yRightBottom)
                ++ (point xB yMiddleBottom)
                ++ "Q "
                ++ (point xA yLeftBottom)
                ++ (point xLeft yLeftBottom)
                ++ "Z"
                )
            ]
            []
            :: acc

{-- Implementation helpers --}


mix : Float -> Color -> Color -> Color
mix p c1 c2 =
  let
    cc1 = Color.toRgb c1
    cc2 = Color.toRgb c2

    m a b = round <| p * (toFloat a) + (1 - p) * (toFloat b)
  in
    Color.rgb (m cc1.red cc2.red) (m cc1.green cc2.green) (m cc1.blue cc2.blue)


bisect : Float -> Float -> (Float -> Float) -> Float
bisect l r f =
  if (r - l) < 0.001 then
      l
  else
      let
        m = (l + r) / 2
      in
        if ((f l) * (f m)) <  0 then
            bisect l m f
        else
            bisect m r f

saturation : Color -> Float
saturation col =
    let
        hsl = Color.toHsl col
    in
        hsl.saturation


calcP2 : Color -> Float
calcP2 col =
  bisect 0.0 1.0 (\p -> ((saturation (mix p col (mix p col Color.white))) - (saturation col)))


defGradient : String -> String -> String -> Svg m
defGradient theId col1 col2 =
    defs
      []
      [ linearGradient
          [ id theId, x1 "0%", y1 "0%", x2 "100%", y2 "0%" ]
          [ stop [offset "0%", Svg.Attributes.style <| "stop-color:" ++ col1 ++ ";stop-opacity:1.0"] []
          , stop [offset "15%", Svg.Attributes.style <| "stop-color:" ++ col1 ++ ";stop-opacity:0.6"] []
          , stop [offset "85%", Svg.Attributes.style <| "stop-color:" ++ col2 ++ ";stop-opacity:0.6"] []
          , stop [offset "100%", Svg.Attributes.style <| "stop-color:" ++ col2 ++ ";stop-opacity:1.0"] []
          ]
      ]



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
    (toString x) ++ " " ++ (toString y) ++ " "


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
