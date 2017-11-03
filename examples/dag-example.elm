module Main exposing (main)

import Dag exposing (Dag, Node, empty, node, mapNodesBfs, mapNodesByRank)
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
    in
        mapNodesByRank (\r n -> (toString r) ++ ": " ++ (toString n)) g
            |> map Html.text
            |> intersperse (Html.br [] [])
            |> Html.body []
