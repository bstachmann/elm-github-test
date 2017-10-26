module Main exposing (main)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import DagRenderer exposing (..)


type Msg
    = Nothing


sample : DagRenderer.StreamLayout String
sample =
    empty 7
        |> appendColumn
        |> appendCell 0 "A" [ 1, 0 ]
        |> appendCell 1 "B" [ 1, 3 ]
        |> appendCell 2 "C" [ 1 ]
        |> appendCell 3 "D" [ 3, 1 ]
        |> appendCell 4 "F" [ 3 ]
        |> appendCell 5 "K" [0]
        |> appendCell 6 "L" [3]
        |> appendColumn
        |> appendCell 0 "A" []
        |> appendCell 1 "B" [ 1 ]
        |> appendCell 3 "F" [ 1, 4 ]
        |> appendColumn
        |> appendCell 1 "B" [ 1 ]
        |> appendCell 4 "G" [ 4 ]
        |> appendColumn
        |> appendCell 1 "H" [ 4 ]
        |> appendCell 4 "I" [ 4 ]
        |> appendColumn
        |> appendCell 4 "J" []


main : Html Msg
main =
    Html.body []
        [ Html.text <| "Hello Stream Graph!"
        , Html.br [] []

        -- , Html.text <| "Layout: " ++ (toString sample)
        , Html.text <| "config: " ++ (toString config)
        , Html.br [] []
        , svg
            [ version "1.1", x "0", y "0", width "1280px", height "2024px", viewBox "0 0 1280px 2024px" ]
          <|
            DagRenderer.newRender sample
        ]
