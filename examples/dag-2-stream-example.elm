module Main exposing (main)

import Dag exposing (Dag, Node, empty, node, mapNodesBfs, mapNodes, getNodeId, mapNodesByRank)
import Html exposing (Html)
import List exposing (intersperse, map)
import DagRenderer exposing (..)
import Dict



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
            |> toString
    in
        idToLane
        |> Html.text
        |> List.singleton
        |> intersperse (Html.br [] [])
        |> Html.body []


type IdToLaneMapping i = EmptyMapping | NewMapping Int (Dict.Dict i Int)

emptyMapping : IdToLaneMapping comparable
emptyMapping = EmptyMapping

mapIdToLane : comparable -> IdToLaneMapping comparable -> IdToLaneMapping comparable
mapIdToLane comparable m =
    case m of
        EmptyMapping
            -> NewMapping 0 <| Dict.singleton comparable 0
        NewMapping maxId previousDict
            -> NewMapping (maxId + 1) <|Dict.insert comparable (maxId + 1) previousDict
