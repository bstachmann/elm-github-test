module Main exposing (main)

import Dict exposing (Dict, get, values)
import Html exposing (Html)
import List exposing (foldl, intersperse, map)
import Maybe
import Set exposing (Set, filter, member, remove)


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
        mapNodes toString g
            |> map Html.text
            |> intersperse (Html.br [] [])
            |> Html.body []


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


payload : Node comparable payload -> payload
payload (NewNode p _) =
    p


mapNodes : (Node comparable payload -> a) -> Dag comparable payload -> List a
mapNodes function (NewDag _ dagNodes _) =
    values dagNodes |> map function
