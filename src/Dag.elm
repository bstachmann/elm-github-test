module Dag exposing (Dag, Node, empty, node, mapNodes, mapNodesBfs)

import Dict exposing (Dict, get, values)
import List exposing (foldl, map, concatMap)
import Maybe
import Set exposing (Set, filter, member, remove)


type Dag comparable payload
    = NewDag (payload -> comparable) (Dict comparable (Node comparable payload)) (Set comparable)


type Node comparable payload
    = NewNode payload (List (Node comparable payload))


empty : (payload -> comparable) -> Dag comparable payload
empty getId =
    NewDag getId Dict.empty Set.empty


node : payload -> List comparable -> Dag comparable payload -> Dag comparable payload
node payload successorIds (NewDag getId dagNodes rootIds) =
    let
        idOfNewNode =
            getId payload

        newRootIds =
            foldl remove rootIds successorIds
                |> Set.insert idOfNewNode

        {--TODO suppresses information about missing keys use Result instead of filtering --}
        successorNodes =
            List.filterMap (\i -> get i dagNodes) successorIds

        newNode =
            NewNode payload successorNodes

        newNodes =
            Dict.insert idOfNewNode newNode dagNodes
    in
        NewDag getId newNodes newRootIds


getPayload : Node comparable payload -> payload
getPayload (NewNode p _) =
    p


successors : Node comparable p -> List (Node comparable p)
successors (NewNode _ s) =
    s


mapNodes : (Node comparable payload -> a) -> Dag comparable payload -> List a
mapNodes function (NewDag _ dagNodes _) =
    values dagNodes |> map function


foldlbreadthFirst : Int -> (Int -> (Node comparable p) -> a -> a) -> a -> Dag comparable p -> a
foldlbreadthFirst rank function acc (NewDag getId nodes rootIds)  =
    let
        (firstRankNodes, remainingNodes) = nodes |> Dict.partition (\i v -> member i rootIds)

        nextAcc = firstRankNodes |> Dict.values |> List.foldl (function rank) acc

        nextRankRootIds =
            firstRankNodes
            |> Dict.values
            |> List.concatMap successors
            |> List.map (\n -> getPayload n |> getId)
            |> Set.fromList
    in
        if not (Dict.isEmpty remainingNodes) then
            {-- TO DO unsauber bc nextRankRootIds arent necessarily roots --}
            foldlbreadthFirst (rank + 1) function nextAcc (NewDag getId remainingNodes nextRankRootIds)
        else
            nextAcc


mapNodesBfs : (Int -> (Node comparable p) -> a) -> Dag comparable p-> List a
mapNodesBfs function dag =
    foldlbreadthFirst 0 (\r n acc -> function r n :: acc) [] dag
