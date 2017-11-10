module DagManipulationUI exposing (..)

import Bootstrap.Grid as Grid
import Dag exposing (Dag, empty, node)
import DagRenderer exposing (StreamLayout, empty, flowGraphWithHeader, toFlowLayout)
import Html exposing (Html, a, div, h5, text)
import Html.Attributes exposing (attribute, class, href, id)
import List exposing (indexedMap)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { dag : Dag String String
    , layouts : List (Transformation String)
    }


type Msg
    = Nothing


type alias Transformation i =
    { layout : StreamLayout i
    }


init : ( Model, Cmd Msg )
init =
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
    in
        ( { dag = g
          , layouts =
                [ { layout = layout1 }
                , { layout = layout1 }
                , { layout = layout1 }
                ]
          }
        , Cmd.none
        )


view : Model -> Html Msg
view model =
    Grid.container
        []
        [ Grid.row
            []
            [ Grid.col
                []
                [ div
                    [ id "accordion"
                    , attribute "role" "tablist"
                    ]
                  <|
                    indexedMap (\i l -> flowGraphCard i (flowGraphWithHeader ( "hallo", l.layout ))) model.layouts
                ]
            ]
        ]


flowGraphCard : Int -> List (Html msg) -> Html msg
flowGraphCard i graphAsHtml =
    div
        [ class "card" ]
        [ div
            [ class "card-header"
            , id "headingOne"
            ]
            [ h5
                [ class "mb-0" ]
                [ a
                    [ attribute "data-toggle" "collapse"
                    , href <| "#collapse" ++ (toString i)
                    , attribute "aria-expanded" "true"
                    , attribute "aria-controls" <| "collapse" ++ (toString i)
                    ]
                    [ text <| "Collapsible Group Item #" ++ (toString i) ]
                ]
            ]
        , div
            [ id <| "collapse" ++ (toString i)
            , class "collapse show"
            , attribute "aria-labelledby" "headingOne"
            ]
            [ div
                [ class "card-body" ]
                graphAsHtml
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nothing ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
