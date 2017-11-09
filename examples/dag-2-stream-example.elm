module Main exposing (main)

import Dag exposing (Dag, Node, empty, foldlByRank, getNodeId, mapNodes, mapNodesBfs, mapNodesByRank, node)
import DagRenderer exposing (..)
import Html exposing (Html)
import List exposing (concatMap, intersperse, map, reverse)


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

        layout1 =
            toFlowLayout g

        ops =
            [ SwapLanes 0 1
            , SwapLanes 4 2
            , SwapLanes 0 3
            , CompressColumns
            ]

        ( layout, layouts ) =
            ops
                |> List.foldl
                    (\op ( l, ls ) ->
                        let
                            newLayout =
                                apply op l
                        in
                            ( newLayout, ( toString op, newLayout ) :: ls )
                    )
                    ( layout1, [ ( "", layout1 ) ] )

        htmls =
            layouts
                |> concatMap flowGraphWithHeader
    in
        Html.body [] <|
            Html.text "Hello Rendering Dag to Stream Graph!"
                :: htmls
