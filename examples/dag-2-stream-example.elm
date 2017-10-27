module Main exposing (main)

import Dag exposing (Dag, Node, empty, node, mapNodesBfs, mapNodes, getNodeId, mapNodesByRank, foldlByRank)
import Html exposing (Html)
import List exposing (intersperse, map)
import DagRenderer exposing (..)
import Dict
import Svg
import Svg.Attributes exposing (..)



type Msg
    = Nothing


main : Html Msg
main =
    let
        g =
            Dag.empty identity
                |> node "A" []
                |> node "B" [ "A" ]
                |> node "C" [ "B" ]
                |> node "D" [ "A" ]
                |> node "E" [ "C", "D" ]
                |> node "F" [ "C" ]

        idToLane =
            Dag.mapNodes (Dag.getNodeId g) g
            |> List.foldl mapIdToLane emptyMapping


        buildStream : Int -> Node String String -> StreamLayout String -> StreamLayout String
        buildStream column node layout =
            let
                l = if column >= (nrOfColumns layout) - 1 then
                      DagRenderer.appendColumn layout
                else
                      layout

                nodeId = Dag.getNodeId g node

            in
                DagRenderer.appendCell (Maybe.withDefault 0 <| laneFor nodeId idToLane) nodeId [] l

        layout = foldlByRank 0  buildStream (DagRenderer.empty 42) g
    in
            Html.body []
                [ Html.text <| "Hello Rendering Dag to Stream Graph!"
                , Html.br [] []

                -- , Html.text <| "Layout: " ++ (toString sample)
                , Html.text <| "config: " ++ (toString config)
                , Html.br [] []
                ,  Svg.svg
                    [ Svg.Attributes.version "1.1", x "0", y "0", width "1280px", height "2024px", viewBox "0 0 1280px 2024px" ]
                  <| DagRenderer.newRender layout
                ]


type IdToLaneMapping comparable = EmptyMapping | NewMapping Int (Dict.Dict comparable Int)

emptyMapping : IdToLaneMapping comparable
emptyMapping = EmptyMapping

mapIdToLane : comparable -> IdToLaneMapping comparable -> IdToLaneMapping comparable
mapIdToLane comparable m =
    case m of
        EmptyMapping
            -> NewMapping 0 <| Dict.singleton comparable 0
        NewMapping maxId previousDict
            -> NewMapping (maxId + 1) <|Dict.insert comparable (maxId + 1) previousDict

laneFor : comparable -> IdToLaneMapping comparable -> Maybe Int
laneFor i m =
    case m of
        (NewMapping _ d)
          -> Dict.get i d
        EmptyMapping
          -> Maybe.Nothing
