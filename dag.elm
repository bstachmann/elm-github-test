module Main exposing (main)

import Array exposing (Array, length, push, fromList)
import Html exposing (Html)
import List exposing (foldl, map)
import Maybe
import Set exposing (Set, filter, insert, member, remove, singleton)


type Msg
    = Nothing


main : Html Msg
main =
    let
        a =
            buildSingleton "A"

        b =
            buildNode "B" [ a ] a

        c =
            buildNode "C" [ b ] b

        d =
            buildNode "D" [ a ] c

        e =
            buildNode "E" [ c, d ] d

        f =
            buildNode "F" [ c ] e
    in
        build f
            |> mapNodes toString
            |> map Html.text
            |> List.intersperse (Html.br [] [])
            |> Html.body []


type DagBuilder payload
    = NewDagBuilder (Node payload) (Dag payload)


type Dag payload
    = NewDag (Array (Node payload)) (Set Int)


type Node payload
    = NewNode Int payload (List (Node payload))


empty : Dag payload
empty =
    NewDag Array.empty Set.empty


buildSingleton : payload -> DagBuilder payload
buildSingleton payload =
    let
        rootNode =
            NewNode 0 payload []

        nodes =
            fromList [ rootNode ]

        rootIndexes =
            singleton 0
    in
        NewDagBuilder rootNode (NewDag nodes rootIndexes)


buildNode : payload -> List (DagBuilder payload) -> DagBuilder payload -> DagBuilder payload
buildNode payload predecessorBuilders (NewDagBuilder _ (NewDag dagNodes rootIndexes)) =
    let
        indexOfNewNode =
            length dagNodes

        predecessors =
            map latest predecessorBuilders

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
        NewDagBuilder newNode (NewDag newNodes newRootIndexes)


indexOf : Node payload -> Int
indexOf (NewNode index _ _) =
    index


payload : Node payload -> payload
payload (NewNode _ p _) =
    p


latest : DagBuilder payload -> Node payload
latest (NewDagBuilder latest _) =
    latest


build : DagBuilder payload -> Dag payload
build (NewDagBuilder _ dag) =
    dag


mapNodes : (Node a -> b) -> Dag a -> List b
mapNodes function (NewDag dagNodes rootIndexes) =
    Array.map function dagNodes |> Array.toList
