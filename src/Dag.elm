module Dag
    exposing
        ( Dag
        , Node
        , SimpleDag
        , empty
        , node
        , mapNodes
        , mapNodesBfs
        , mapNodesByRank
        , getNodeId
        , foldlByRank
        , rootIds
        , successors
        )

{-| A library for creating and traversing DAGs (directed acyclic graph).

# Creation
@docs empty

# Traversal
@docs mapNodes

-}


import Dict exposing (Dict, get, values)
import List exposing (concatMap, foldl, map)
import Maybe
import Set exposing (Set, filter, member, remove)


-- Core Types

{-| Represents a directed acyclic graph (DAG).
Each Node can carry a payload of type `p` and has to provide a uniquie identifier of type `i`.  --}

type Dag i p
    = LinkedNodesDag -- A graph representation where nodes reference their successore directly.

        -- Function to extract the id from a nodes payload
        (p -> i)

        -- Map to store and lookup all nodes by their id
        (Dict i (Node i p))

        -- Ids of source nodes as entrypoint for traversal algorithms
        (Set i)

{-| Represents a Node in DAG carrying a payload of type `p`.

The payload may be an arbitrarily complex data structure, e. g.  a record. --}

type Node comparable p
    = LinkedNode

        -- Payload of data attached to this node
        p

        -- All successors nodes that are reachable by a single edge from this noder
        (List (Node comparable p))


{-| A simple Dag is one where the nodes can be uniquely identified with `toString` on their payload  --}

type alias SimpleDag p = Dag String p


-- CREATION

empty : (payload -> comparable) -> Dag comparable payload
empty getId =
    LinkedNodesDag getId Dict.empty Set.empty


node : payload -> List comparable -> Dag comparable payload -> Dag comparable payload
node payload successorIds (LinkedNodesDag getId dagNodes rootIds) =
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
            LinkedNode payload successorNodes

        newNodes =
            Dict.insert idOfNewNode newNode dagNodes
    in
        LinkedNodesDag getId newNodes newRootIds


getPayload : Node comparable payload -> payload
getPayload (LinkedNode p _) =
    p


getNodeId : Dag comparable payload -> Node comparable payload -> comparable
getNodeId (LinkedNodesDag getId _ _) node =
    getId <| getPayload node


rootIds : Dag comparable payload -> Set comparable
rootIds (LinkedNodesDag _ _ rootIds) =
    rootIds


successors : Node comparable p -> List (Node comparable p)
successors (LinkedNode _ s) =
    s


mapNodes : (Node comparable payload -> a) -> Dag comparable payload -> List a
mapNodes function (LinkedNodesDag _ dagNodes _) =
    values dagNodes |> map function


foldlbreadthFirst : Int -> (Int -> Node comparable p -> a -> a) -> a -> Dag comparable p -> a
foldlbreadthFirst rank function acc (LinkedNodesDag getId nodes rootIds) =
    let
        ( firstRankNodes, remainingNodes ) =
            nodes |> Dict.partition (\i v -> member i rootIds)

        nextAcc =
            firstRankNodes |> Dict.values |> foldl (function rank) acc

        nextRankRootIds =
            firstRankNodes
                |> Dict.values
                |> concatMap successors
                |> map (\n -> getPayload n |> getId)
                |> Set.fromList
    in
        if not (Dict.isEmpty remainingNodes) then
            {--TO DO unsauber bc nextRankRootIds arent necessarily roots --}
            foldlbreadthFirst (rank + 1) function nextAcc (LinkedNodesDag getId remainingNodes nextRankRootIds)
        else
            nextAcc


mapNodesBfs : (Int -> Node comparable p -> a) -> Dag comparable p -> List a
mapNodesBfs function dag =
    foldlbreadthFirst 0 (\r n acc -> function r n :: acc) [] dag


foldlByRank : Int -> (Int -> Node comparable p -> a -> a) -> a -> Dag comparable p -> a
foldlByRank rank function acc (LinkedNodesDag getId nodes rootIds) =
    let
        ( rootNodes, remainingNodes ) =
            nodes |> Dict.partition (\i v -> member i rootIds)

        nextAcc =
            rootNodes |> Dict.values |> foldl (function rank) acc

        nextRankNodes =
            Dict.values rootNodes
                |> concatMap successors

        nextRankIds =
            nextRankNodes |> map (getPayload >> getId) |> Set.fromList

        overnextRankIds =
            nextRankNodes
                |> concatMap successors
                |> map (getPayload >> getId)
                |> Set.fromList

        nextRootIds =
            Set.diff nextRankIds overnextRankIds
    in
        if not (Dict.isEmpty remainingNodes) then
            foldlByRank (rank + 1) function nextAcc (LinkedNodesDag getId remainingNodes nextRootIds)
        else
            nextAcc


mapNodesByRank : (Int -> Node comparable p -> a) -> Dag comparable p -> List a
mapNodesByRank function dag =
    foldlByRank 0 (\r n acc -> function r n :: acc) [] dag
