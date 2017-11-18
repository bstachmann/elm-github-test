module DagManipulationUI exposing (..)

import Array exposing (Array)
import Bootstrap.Form exposing (label)
import Bootstrap.Grid as Grid
import Dag exposing (Dag, empty, node)
import DagRenderer exposing (..)
import Html exposing (Html, a, div, h5, input, text)
import Html.Attributes exposing (attribute, class, href, id, value)
import Html.Events exposing (onInput)
import List exposing (foldl, indexedMap)
import String exposing (toInt)


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
    , transformations : Array (Transformation String)
    }


type Msg
    = Nothing
    | UpdateTransformation Int (Result String (DagRenderer.Dsl String))


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
                Array.fromList
                    [ { transformation = DagRenderer.Identity }
                    , { transformation = DagRenderer.SwapLanes 1 3 }
                    , { transformation = DagRenderer.CompressColumns }
                    , { transformation = DagRenderer.SwapLanes 0 1 }
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


flowGraphCards : Model -> List (Html Msg)
flowGraphCards model =
    model.transformations
        |> Array.foldl
            (\t ( previousLayout, results ) ->
                let
                    l =
                        DagRenderer.apply t.transformation previousLayout
                in
                    ( l, (( t, l ) :: results) )
            )
            ( model.initialLayout, [] )
        |> Tuple.second
        |> List.reverse
        |> indexedMap (\i ( t, l ) -> flowGraphCard i t l)


flowGraphCard : Int -> Transformation String -> StreamLayout String -> Html Msg
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
                    , transformationView i t
                    ]
                ]
            , div
                [ id bodyCollapseId
                , class "collapse hide"
                , attribute "aria-labelledby" cardHeaderId
                ]
                [ div
                    [ class "card-body" ]
                    ((flowGraphWithHeader
                        ("flow" ++ (toString i))
                        ( "egal", l )
                     )
                    )
                ]
            ]


transformationView : Int -> Transformation String -> Html Msg
transformationView i t =
    case t.transformation of
        CompressColumns ->
            h5 [] [ text ("wurst" ++ toString t) ]

        DagRenderer.SwapCells _ _ _ ->
            h5 [] [ text ("kaese" ++ toString t) ]

        DagRenderer.SwapLanes l1 l2 ->
            Bootstrap.Form.form
                []
                [ div [ class "form-group" ]
                    [ label [] [ text "Lane 1" ]
                    , input
                        [ value <| toString l1, onInputUpdateTransformation i toInt (\l -> (SwapLanes l l2)) ]
                        []
                    ]
                , div [ class "form-group" ]
                    [ label [] [ text "Lane 2" ]
                    , input
                        [ value <| toString l2, onInputUpdateTransformation i toInt (\l -> (SwapLanes l1 l)) ]
                        []
                    ]
                ]

        DagRenderer.Identity ->
            h5 [] [ text ("gouda" ++ toString t) ]


onInputUpdateTransformation :
    Int
    -> (String -> Result String a)
    -> (a -> Dsl String)
    -> Html.Attribute Msg
onInputUpdateTransformation i parseString createTransformation =
    onInput
        (\s ->
            s
                |> parseString
                |> Result.andThen (createTransformation >> Ok)
                |> UpdateTransformation i
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nothing ->
            ( model, Cmd.none )

        UpdateTransformation nr maybeCommand ->
            ( maybeCommand
                |> Result.andThen
                    (\command ->
                        Result.Ok { model | transformations = Array.set nr { transformation = command } model.transformations }
                    )
                |> Result.withDefault model
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
