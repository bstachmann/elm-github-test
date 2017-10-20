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
        g0 = Dag (NewNode -1 "nix" []) Array.empty Set.empty
        gA = node "A" [] g0
        gB = node "B" [latest gA] gA
        gC = node "C" [latest gB] gB
        gD = node "D" [latest gA] gC
        gE = node "E" [latest gC, latest gD] gD
        gF = node "F" [latest gC] gE
    in
      mapNodes toString gF
      |> map Html.text
      |> List.intersperse (Html.br [] [])
      |>  Html.body []


type Dag payload
    = Dag (Node payload) (Array (Node payload)) (Set Int)


type Node payload
    = NewNode Int payload (List (Node payload))


node : payload -> List (Node payload) -> Dag payload -> Dag payload
node payload predecessors (Dag latest dagNodes rootIndexes) =
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
        Dag newNode newNodes newRootIndexes


indexOf : Node payload -> Int
indexOf (NewNode index _ _) =
    index


payload : Node payload -> payload
payload (NewNode _ p _) =
    p

latest : Dag payload -> Node payload
latest (Dag latest _ _) =
    latest


mapNodes : (Node a -> b) -> Dag a -> List b
mapNodes function (Dag latest dagNodes rootIndexes) =
    Array.map function dagNodes |> Array.toList
