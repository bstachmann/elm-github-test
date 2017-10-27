module Main exposing (main)

import Dag exposing (Dag, LinkedNode, empty, node, mapNodesBfs, mapNodesByRank)
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
            empty identity
                |> node "A" []
                |> node "B" [ "A" ]
                |> node "C" [ "B" ]
                |> node "D" [ "A" ]
                |> node "E" [ "C", "D" ]
                |> node "F" [ "C" ]
    in
        mapNodesByRank (\r n -> (toString r) ++ ": " ++ (toString n)) g
            |> map Html.text
            |> intersperse (Html.br [] [])
            |> Html.body []


type IdToLaneMapping i = EmptyMapping | NewMapping Int (Dict.Dict i Int)

empty : IdToLaneMapping
empty = EmptyMapping

mapIdToLane : i -> IdToLaneMapping -> IdToLaneMapping
mapIdToLane i m =
    case m of
        Empty
            -> NewMapping 0 <| Dict.singleton i 0
        NewMapping maxId Dict as previousDict
            -> NewMapping (maxId + 1) <| Dict.insert i (maxId + 1) previousDict
