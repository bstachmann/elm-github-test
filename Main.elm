module Main exposing (main)

import Html exposing (..)
import Bootstrap.Grid as Grid


main =
    Grid.container []
        [ Grid.row []
            [ Grid.col [] [ text "Moin Moin"]
            , Grid.col [] [ text "Hallo Hallo"]
            ]
          , Grid.row []
              [ Grid.col [] [ text "Tach"]
              , Grid.col [] [ text "auch"]
          ]
        ]
