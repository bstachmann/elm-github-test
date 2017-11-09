module DagRenderer exposing (..)

import Array exposing (Array)
import Dag exposing (Dag, Node)
import Dict exposing (Dict, get, insert, keys, update)
import Html exposing (Html, br, text)
import List exposing (append, concatMap, drop, filterMap, foldl, head, indexedMap, map, maximum, range)
import Maybe exposing (withDefault)
import Set
import Svg exposing (..)
import Svg.Attributes exposing (..)


{--Types --}


type Cell i
    = NewCell i (List Int)



-- IMPROVE Check if int-dict works here


type alias ColumnDict i =
    Dict Int (Cell i)


type StreamLayout i
    = NewStreamLayout (Dict Int (ColumnDict i))


type alias ColumnId =
    Int


type alias LaneId =
    Int



{--Building layouts --}


empty : StreamLayout i
empty =
    NewStreamLayout Dict.empty


appendColumn : StreamLayout i -> StreamLayout i
appendColumn (NewStreamLayout columnToColdict) =
    let
        nextData =
            Dict.insert (Dict.size columnToColdict) Dict.empty columnToColdict
    in
        NewStreamLayout nextData


appendCell : LaneId -> i -> List Int -> StreamLayout i -> StreamLayout i
appendCell lane i successors (NewStreamLayout columnToColdict) =
    let
        lastColumn =
            Dict.size columnToColdict - 1

        newCell =
            NewCell i successors

        insertCellIntoColumn previousColDict =
            previousColDict
                |> withDefault Dict.empty
                |> insert lane newCell
                |> Just
    in
        NewStreamLayout <|
            Dict.update lastColumn (insertCellIntoColumn) columnToColdict



{--a DSL for layout manipulation --}


type Dsl i
    = CompressColumns
    | SwapCells Int Int Int
    | SwapLanes Int Int


apply : Dsl i -> StreamLayout i -> StreamLayout i
apply command layout =
    layout
        |> case command of
            CompressColumns ->
                compressColumns

            SwapCells l1 l2 column ->
                swapCells l1 l2 column

            SwapLanes l1 l2 ->
                swapLanes l1 l2



{--Access --}


nrOfColumns : StreamLayout i -> Int
nrOfColumns (NewStreamLayout columnToColdict) =
    Dict.size columnToColdict


nrOfLanes : StreamLayout i -> Int
nrOfLanes (NewStreamLayout columnToColdict) =
    Dict.values columnToColdict
        |> filterMap (keys >> maximum)
        |> maximum
        |> withDefault 0



{--Mainipulating Layouts --}


swapLanes : Int -> Int -> StreamLayout i -> StreamLayout i
swapLanes lane1 lane2 ((NewStreamLayout columnToColdict) as layout) =
    Dict.keys columnToColdict
        |> List.foldl (swapCells lane1 lane2) layout


swapCells : Int -> Int -> Int -> StreamLayout i -> StreamLayout i
swapCells lane1 lane2 column ((NewStreamLayout columnToColdict) as previousLayout) =
    case Dict.get column columnToColdict of
        Nothing ->
            previousLayout

        Just colDict ->
            let
                nextColumnDict =
                    colDict
                        |> update lane2 (\_ -> get lane1 colDict)
                        |> update lane1 (\_ -> get lane2 colDict)
            in
                columnToColdict
                    |> insert column nextColumnDict
                    |> update (column - 1) (Maybe.map (remapLinksInColumnToTheLeft lane1 lane2))
                    |> NewStreamLayout


remapSucc : Int -> Int -> Int -> Int
remapSucc lane1 lane2 lane =
    if lane == lane1 then
        lane2
    else if lane == lane2 then
        lane1
    else
        lane


remapLinksInColumnToTheLeft : Int -> Int -> ColumnDict i -> ColumnDict i
remapLinksInColumnToTheLeft lane1 lane2 cd =
    Dict.map (\k (NewCell i succs) -> (NewCell i (List.map (remapSucc lane1 lane2) succs))) cd


tranposeCell : Int -> Cell i -> Cell i
tranposeCell offset (NewCell i succs) =
    List.map ((+) offset) succs
        |> NewCell i


compressColumns : StreamLayout i -> StreamLayout i
compressColumns ((NewStreamLayout columnToColdict) as layout) =
    keys columnToColdict
        |> foldl (\c -> compressColumn c) layout


swappingPairs : Maybe (ColumnDict i) -> List ( Int, Int )
swappingPairs columnDict =
    columnDict
        |> withDefault Dict.empty
        -- todo error handling
        |> keys
        -- IMPROVE filter idempotet swaps
        |> indexedMap (\i k -> ( i, k ))


compressColumn : Int -> StreamLayout i -> StreamLayout i
compressColumn column ((NewStreamLayout columnToColdict) as layout) =
    swappingPairs (get column columnToColdict)
        |> List.foldl (\( i, k ) -> swapCells i k column) layout



{--Rendering to SVG --}


config =
    { columnWidth = 120
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
        let
            n =
                8
        in
            range 0 (n - 1)
                |> List.map
                    (\i -> "hsl(" ++ (toString (i * 360 // n)) ++ ",90%,90%)")
                |> Array.fromList
    , opacity = "0.7"
    }


flowGraphWithHeader : ( String, StreamLayout i ) -> List (Html m)
flowGraphWithHeader ( desc, layout ) =
    let
        h =
            nrOfLanes layout |> (+) 1 |> (*) config.rowHeight |> toString |> (\s -> s ++ "px")
    in
        [ Html.br [] []
        , Html.text <| "Graph: " ++ desc ++ " / " ++ h
        , Html.br [] []
        , Svg.svg
            [ Svg.Attributes.version "1.1", x "0", y "0", width "1280px", height h, viewBox ("0 0 1280px " ++ h) ]
          <|
            newRender layout
        ]


newRender : StreamLayout i -> List (Svg m)
newRender ((NewStreamLayout columnToColdict) as layout) =
    (range 0 ((nrOfColumns layout) - 1)
        |> List.foldl (renderColumn layout) []
    )


renderColumn : StreamLayout i -> ColumnId -> List (Svg m) -> List (Svg m)
renderColumn ((NewStreamLayout columnToColdict) as layout) column acc =
    let
        x0 =
            config.columnWidth * column

        lanes =
            Dict.get column columnToColdict
                |> Maybe.withDefault Dict.empty
                |> Dict.keys
    in
        acc
            |> (\acc -> foldl (renderSection layout column) acc lanes)


renderSection : StreamLayout i -> ColumnId -> LaneId -> List (Svg m) -> List (Svg m)
renderSection ((NewStreamLayout columnToColdict) as layout) column lane acc =
    Dict.get column columnToColdict
        |> Maybe.andThen (Dict.get lane)
        |> Maybe.map (\c -> renderCell layout column lane c acc)
        |> Maybe.withDefault acc


renderCell : StreamLayout i -> Int -> Int -> Cell i -> List (Svg m) -> List (Svg m)
renderCell ((NewStreamLayout _) as layout) column lane (NewCell i successors) acc =
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
                [ Svg.text <| toString i ]
            :: acc
            |> (\acc -> List.foldl (newRenderConnections layout column lane) acc successors)
            |> diagnostic "cell" lane "red" bounds


newRenderConnections : StreamLayout i -> Int -> Int -> LaneId -> List (Svg m) -> List (Svg m)
newRenderConnections ((NewStreamLayout columnToColdict) as layout) column laneLeft laneRight acc =
    let
        xRight =
            (column + 1) * config.columnWidth

        xLeft =
            xRight - config.connectorWidth

        xA =
            xLeft + ((config.connectorWidth * 1) // 4)

        xB =
            xLeft + ((config.connectorWidth * 2) // 4)

        xC =
            xLeft + ((config.connectorWidth * 3) // 4)

        yLeftTop =
            laneLeft * config.rowHeight

        yLeftBottom =
            yLeftTop + config.laneHeight

        yRightTop =
            laneRight * config.rowHeight

        yRightBottom =
            yRightTop + config.laneHeight

        yMiddleTop =
            (yLeftTop + yRightTop) // 2

        yMiddleBottom =
            (yLeftBottom + yRightBottom) // 2

        gradientId =
            "gr" ++ (toString laneLeft) ++ "_" ++ (toString laneRight)
    in
        defGradient gradientId (colorForLane laneLeft) (colorForLane laneRight)
            :: Svg.path
                [ fill <| "url(#" ++ gradientId ++ ")"
                , d
                    (-- Move right
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



{--Implementation helpers --}


defGradient : String -> String -> String -> Svg m
defGradient theId col1 col2 =
    defs
        []
        [ linearGradient
            [ id theId, x1 "0%", y1 "0%", x2 "100%", y2 "0%" ]
            [ stop [ offset "0%", Svg.Attributes.style <| "stop-color:" ++ col1 ++ ";stop-opacity:1.0" ] []
            , stop [ offset "30%", Svg.Attributes.style <| "stop-color:" ++ col1 ++ ";stop-opacity:0.6" ] []
            , stop [ offset "85%", Svg.Attributes.style <| "stop-color:" ++ col2 ++ ";stop-opacity:0.6" ] []
            , stop [ offset "100%", Svg.Attributes.style <| "stop-color:" ++ col2 ++ ";stop-opacity:1.0" ] []
            ]
        ]


colorForLane : LaneId -> String
colorForLane lane =
    Array.get ((lane * 3) % (Array.length config.laneColors)) config.laneColors |> Maybe.withDefault "blue"


diagnostic : String -> e -> String -> ( Int, Int, Int, Int ) -> List (Svg m) -> List (Svg m)
diagnostic t msg color_ (( x0, y0, _, _ ) as bounds) acc =
    if Set.member t config.diagnosticsFor then
        rect ([ stroke color_, strokeOpacity "0.5", fill "none" ] |> inBox bounds) []
            :: Svg.text_ [ x (toString (x0 + 2)), y (toString (y0 + 12)), fill color_, fillOpacity "0.5" ] [ Svg.text (toString msg) ]
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



{--Dag to Layout helpers --}


buildStream : IdToLaneMapping String -> Dag String String -> Int -> Node String String -> ( Set.Set String, StreamLayout String ) -> ( Set.Set String, StreamLayout String )
buildStream idToLane g column node ( openNodeIds, layout ) =
    let
        l =
            if column >= (nrOfColumns layout) - 1 then
                -- doppelt ?
                openNodeIds |> Set.foldl (\i acc -> appendCell (laneFor i idToLane |> Maybe.withDefault 0) i [ (laneFor i idToLane |> Maybe.withDefault 0) ] acc) (appendColumn layout)
            else
                layout

        nodeId =
            Dag.getNodeId g node

        -- improve evil filter
        succesorNodeIds =
            Dag.successors node |> List.map (Dag.getNodeId g)

        successorLaneIds =
            succesorNodeIds |> List.filterMap (\i -> laneFor i idToLane)

        nextOpenIds =
            Set.remove nodeId openNodeIds
                |> Set.union (Set.fromList succesorNodeIds)

        nextLayout =
            appendCell (Maybe.withDefault 0 <| laneFor nodeId idToLane) nodeId successorLaneIds l
    in
        ( nextOpenIds, nextLayout )


type IdToLaneMapping comparable
    = EmptyMapping
    | NewMapping Int (Dict.Dict comparable Int)


emptyMapping : IdToLaneMapping comparable
emptyMapping =
    EmptyMapping


mapIdToLane : comparable -> IdToLaneMapping comparable -> IdToLaneMapping comparable
mapIdToLane comparable m =
    case m of
        EmptyMapping ->
            NewMapping 0 <| Dict.singleton comparable 0

        NewMapping maxId previousDict ->
            NewMapping (maxId + 1) <| Dict.insert comparable (maxId + 1) previousDict


laneFor : comparable -> IdToLaneMapping comparable -> Maybe Int
laneFor i m =
    case m of
        NewMapping _ d ->
            Dict.get i d

        EmptyMapping ->
            Maybe.Nothing



{--Deprecated Stuff --}


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
        :: text_ [ x (toString section.x0), y (toString section.lane.y1) ] [ Svg.text <| "<" ++ section.id ++ ">" ]
        :: (concatMap (renderConnection section) <| List.reverse <| section.predecessor_lanes)
