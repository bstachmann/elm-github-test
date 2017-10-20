module Dag exposing (main)

import Array exposing (Array, length, push)
import Html exposing (Html)
import List exposing (foldl, map)
import Maybe
import Set exposing (Set, filter, insert, member, remove)


type Msg
    = Nothing


main : Html Msg
main =
    let
        greeting = "Hello, WoWO"
        g0 = Dag Set.empty Array.empty
        g1 = node "A" [] g0
    in
      mapNodes toString g1
      |> map Html.text
      |>  Html.body []


type Dag payload
    = Dag (Set Int) (Array (Node payload))


type Node payload
    = NewNode Int payload (List (Node payload))


node : payload -> List (Node payload) -> Dag payload -> Dag payload
node payload predecessors (Dag rootIndexes dagNodes) =
    let
        indexOfNewNode =
            length dagNodes

        predecessorIndexes =
            map indexOf predecessors

        newRootIndexes =
            foldl remove rootIndexes predecessorIndexes
                |> insert indexOfNewNode

        newNode =
            NewNode indexOfNewNode payload predecessors

        newNodes =
            push newNode dagNodes
    in
        Dag newRootIndexes newNodes


indexOf : Node payload -> Int
indexOf (NewNode index _ _) =
    index



payload : Node payload -> payload
payload (NewNode _ p _) =
    p



mapNodes : (Node a -> b) -> Dag a -> List b
mapNodes function (Dag rootIndexes dagNodes) =
    Array.map function dagNodes |> Array.toList
