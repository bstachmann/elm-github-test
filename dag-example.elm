module Main exposing (main)

import Dag exposing (Dag, Node, empty, node, mapNodes)
import Html exposing (Html)
import List exposing (intersperse, map)


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
