module DagManipulationUI exposing (..)

import Array exposing (Array, push)
import Array.Extra exposing (sliceFrom, sliceUntil, splitAt)
import Bootstrap.Accordion as Accordion exposing (State)
import Bootstrap.Button exposing (button, disabled, onClick, primary)
import Bootstrap.Card as Card
import Bootstrap.Form exposing (formInline, label)
import Bootstrap.Form.Input exposing (defaultValue, small, text)
import Bootstrap.Form.InputGroup exposing (..)
import Bootstrap.Grid as Grid exposing (col, containerFluid, row)
import Dag exposing (Dag, empty, node)
import DagRenderer exposing (..)
import Html exposing (Html, a, div, h5, input, text)
import Html.Attributes
import List exposing (foldl, indexedMap)
import String exposing (toInt)
import Svg.Attributes exposing (mode)


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
    , transformationAccordionState : Accordion.State
    }


type Msg
    = Nothing
    | UpdateTransformation Int (Result String (DagRenderer.Dsl String))
    | DeleteTransformation Int
    | AddTransformationAfter Int (Dsl String)
    | AccordionMessage Accordion.State


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
          , transformationAccordionState = Accordion.initialState
          }
        , Cmd.none
        )


view : Model -> Html Msg
view model =
    containerFluid
        []
        [ row
            []
            [ col [] [ transformationAccordionView model ] ]
        ]


transformationAccordionView : Model -> Html Msg
transformationAccordionView model =
    Accordion.config (\state -> AccordionMessage state)
        |> Accordion.withAnimation
        |> Accordion.cards (flowGraphCards model)
        |> Accordion.view model.transformationAccordionState


flowGraphCards : Model -> List (Accordion.Card Msg)
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


flowGraphCard : Int -> Transformation String -> StreamLayout String -> Accordion.Card Msg
flowGraphCard i t l =
    Accordion.card
        { id = "card" ++ (toString i) -- IMPROVE use global identifier, so we can handle multiple accordions.
        , options = []
        , header =
            Accordion.prependHeader
                [ formInline [] (transformationView i t) ]
            <|
                Accordion.header [] (Accordion.toggle [] [ Html.text "Show" ])
        , blocks =
            [ Accordion.block
                []
                [ Card.custom
                    (div [ Html.Attributes.class "img-fluid" ] <| flowGraphWithHeader ("flow" ++ (toString i)) ( "egal", l ))
                , Card.custom <|
                    formInline []
                        [ actionButton (i > 0) "Delete" (DeleteTransformation i)
                        , addTransformationButton i CompressColumns
                        , addTransformationButton i (SwapLanes 0 0)
                        , addTransformationButton i (SwapCells 0 0 0)
                        ]
                ]
            ]
        }


actionButton : Bool -> String -> Msg -> Html Msg
actionButton enabled label msg =
    Bootstrap.Button.button
        [ Bootstrap.Button.secondary
        , onClick
            (if enabled then
                msg
             else
                Nothing
            )
        , disabled (not enabled)
        ]
        [ Html.text label ]


addTransformationButton : Int -> Dsl String -> Html Msg
addTransformationButton i t =
    actionButton True ("+" ++ (toString t)) (AddTransformationAfter i t)


transformationView : Int -> Transformation String -> List (Html Msg)
transformationView i t =
    Html.div [] [ Html.text <| toString t.transformation ]
        :: case t.transformation of
            CompressColumns ->
                []

            DagRenderer.SwapCells _ _ _ ->
                []

            DagRenderer.SwapLanes l1 l2 ->
                [ intField i "1st lane " l1 (\l -> (SwapLanes l l2))
                , intField i "2nd lane " l2 (\l -> (SwapLanes l1 l))
                ]

            DagRenderer.Identity ->
                []


intField : Int -> String -> a -> (Int -> Dsl String) -> Html Msg
intField i labelText currentValue updateTransformation =
    Bootstrap.Form.InputGroup.config
        (Bootstrap.Form.InputGroup.number
            [ defaultValue (toString currentValue)
            , onInputUpdateTransformation i toInt updateTransformation
            ]
        )
        |> Bootstrap.Form.InputGroup.predecessors
            [ Bootstrap.Form.InputGroup.span [] [ Html.text labelText ] ]
        |> Bootstrap.Form.InputGroup.view


onInputUpdateTransformation :
    Int
    -> (String -> Result String a)
    -> (a -> Dsl String)
    -> Bootstrap.Form.Input.Option Msg
onInputUpdateTransformation i parseString createTransformation =
    Bootstrap.Form.Input.onInput
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

        AccordionMessage state ->
            ( { model | transformationAccordionState = state }, Cmd.none )

        DeleteTransformation i ->
            ( { model | transformations = Array.Extra.removeAt i model.transformations }, Cmd.none )

        AddTransformationAfter i t ->
            ( { model
                | transformations =
                    let
                        ( l, r ) =
                            splitAt (i + 1) model.transformations
                    in
                        Array.append (push { transformation = t } l) r
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Accordion.subscriptions model.transformationAccordionState AccordionMessage
