module Main exposing (main)

import Dag exposing (Dag, Node, empty, node, mapNodesBfs, mapNodes, getNodeId, mapNodesByRank, foldlByRank)
import Html exposing (Html)
import List exposing (intersperse, map)
import DagRenderer exposing (..)
import Dict
import Svg
import Svg.Attributes exposing (..)
import Set



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

        idToLane =
            Dag.mapNodes (Dag.getNodeId g) g
            |> List.foldl mapIdToLane emptyMapping


        (_,layout1) = foldlByRank 0 (buildStream idToLane g) ( Dag.rootIds g, DagRenderer.empty) g

        layout2 =
            layout1
            |> swapCells 3 0 2
            |> swapCells 3 2 5
    in
            Html.body []
                [ Html.text <| "Hello Rendering Dag to Stream Graph!"
                , Html.br [] []

                -- , Html.text <| "Layout: " ++ (toString sample)
                , Html.text <| "config: " ++ (toString config)
                , Html.br [] []
                ,  Svg.svg
                    [ Svg.Attributes.version "1.1", x "0", y "0", width "1280px", height "400px", viewBox "0 0 1280px 2024px" ]
                    <| DagRenderer.newRender layout1
                , Html.br [] []
                , Html.text <| "Achtung: "
                , Html.br [] []
                ,  Svg.svg
                    [ Svg.Attributes.version "1.1", x "0", y "0", width "1280px", height "400px", viewBox "0 0 1280px 2024px" ]
                    <| DagRenderer.newRender layout2
                ]



buildStream : (IdToLaneMapping String) -> Dag String String -> Int -> Node String String -> (Set.Set String, StreamLayout String) -> (Set.Set String, StreamLayout String)
buildStream idToLane g column node (openNodeIds, layout) =
   let


      l = if column >= (nrOfColumns layout) - 1 then
            -- doppelt ?
            openNodeIds |> Set.foldl (\i acc -> DagRenderer.appendCell (laneFor i idToLane |> Maybe.withDefault 0) i [(laneFor i idToLane |> Maybe.withDefault 0)] acc) (DagRenderer.appendColumn layout)
      else
            layout

      nodeId = Dag.getNodeId g node

      -- improve evil filter
      succesorNodeIds =
          Dag.successors node |> List.map (getNodeId g)

      successorLaneIds =
          succesorNodeIds |> List.filterMap (\i -> laneFor i idToLane)

      nextOpenIds =
         Set.remove nodeId openNodeIds
         |> Set.union (Set.fromList succesorNodeIds)

      nextLayout =
          DagRenderer.appendCell (Maybe.withDefault 0 <| laneFor nodeId idToLane) nodeId successorLaneIds l

  in
      (nextOpenIds, nextLayout)



type IdToLaneMapping comparable = EmptyMapping | NewMapping Int (Dict.Dict comparable Int)

emptyMapping : IdToLaneMapping comparable
emptyMapping = EmptyMapping

mapIdToLane : comparable -> IdToLaneMapping comparable -> IdToLaneMapping comparable
mapIdToLane comparable m =
    case m of
        EmptyMapping
            -> NewMapping 0 <| Dict.singleton comparable 0
        NewMapping maxId previousDict
            -> NewMapping (maxId + 1) <|Dict.insert comparable (maxId + 1) previousDict

laneFor : comparable -> IdToLaneMapping comparable -> Maybe Int
laneFor i m =
    case m of
        (NewMapping _ d)
          -> Dict.get i d
        EmptyMapping
          -> Maybe.Nothing
