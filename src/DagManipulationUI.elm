module DagManipulationUI exposing (..)

import Bootstrap.Grid as Grid
import Dag exposing (Dag, empty, node)
import DagRenderer exposing (StreamLayout, empty, flowGraphWithHeader, toFlowLayout)
import Html exposing (Html, a, div, h5, text)
import Html.Attributes exposing (attribute, class, href, id)
import List exposing (foldl, indexedMap)


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
    , initialLayout : StreamLayout String
    , transformations : List (Transformation String)
    }


type Msg
    = Nothing


type alias Transformation i =
    { transformation : DagRenderer.Dsl i
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
    in
        ( { dag = g
          , initialLayout = toFlowLayout g
          , transformations =
                [ { transformation = DagRenderer.CompressColumns }
                , { transformation = DagRenderer.SwapLanes 1 3 }
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
                    flowGraphCards model
                ]
            ]
        ]


flowGraphCards : Model -> List (Html msg)
flowGraphCards model =
    model.transformations
        |> foldl
            (\t ( previousLayout, results ) ->
                let
                    l =
                        DagRenderer.apply t.transformation previousLayout
                in
                    ( l, (( t, l ) :: results) )
            )
            ( model.initialLayout, [] )
        |> Tuple.second
        |> indexedMap (\i ( t, l ) -> flowGraphCard i t l)


flowGraphCard : Int -> Transformation String -> StreamLayout String -> Html msg
flowGraphCard i t l =
    let
        -- IMPROVE make unique if multiple instances of this graph are active
        transformationId =
            toString i

        cardHeaderId =
            (toString t.transformation)

        bodyCollapseId =
            "transformationBodyBodyCollapse" ++ (toString i)
    in
        div
            [ class "card" ]
            [ div
                [ class "card-header"
                , id cardHeaderId
                ]
                [ h5
                    [ class "mb-0" ]
                    [ a
                        [ attribute "data-toggle" "collapse"
                        , href <| "#" ++ bodyCollapseId
                        , attribute "aria-expanded" "true"
                        , attribute "aria-controls" bodyCollapseId
                        ]
                        [ text cardHeaderId ]
                    ]
                ]
            , div
                [ id bodyCollapseId
                , class "collapse hide"
                , attribute "aria-labelledby" cardHeaderId
                ]
                [ div
                    [ class "card-body" ]
                    (flowGraphWithHeader
                        ("flow" ++ (toString i))
                        ( "egal", l )
                    )
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
