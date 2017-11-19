module DagManipulationUI exposing (..)

import Array exposing (Array)
import Bootstrap.Accordion as Accordion exposing (State)
import Bootstrap.Card as Card
import Bootstrap.Form exposing (label)
import Bootstrap.Form.Fieldset exposing (..)
import Bootstrap.Form.Input exposing (defaultValue, small, text)
import Bootstrap.Form.InputGroup exposing (..)
import Bootstrap.Grid as Grid
import Dag exposing (Dag, empty, node)
import DagRenderer exposing (..)
import Html exposing (Html, a, div, h5, input, text)
import Html.Attributes exposing (attribute, class, href, id, value)
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
    , transformationAccordionState : Accordion.State
    }


type Msg
    = Nothing
    | UpdateTransformation Int (Result String (DagRenderer.Dsl String))
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
    Grid.containerFluid
        []
        [ Grid.row
            []
            [ Grid.col
                []
                [ transformationAccordionView model ]
            ]
        , Grid.row
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


transformationAccordionView : Model -> Html Msg
transformationAccordionView model =
    Accordion.config (\state -> AccordionMessage state)
        |> Accordion.withAnimation
        |> Accordion.cards
            [ Accordion.card
                { id = "card1"
                , options = []
                , blocks = [ Accordion.block [] [ Card.text [] [ Html.text "Content" ] ] ]
                , header = Accordion.header [] <| Accordion.toggle [] [ Html.text "Juhu" ]
                }
            ]
        |> Accordion.view model.transformationAccordionState


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
            []
            [ div
                [ class "card-header"
                , id cardHeaderId
                ]
                [ Bootstrap.Form.formInline
                    []
                    [ Bootstrap.Form.Fieldset.config
                        |> Bootstrap.Form.Fieldset.children (collapseButton bodyCollapseId :: transformationView i t)
                        |> Bootstrap.Form.Fieldset.view
                    ]
                ]
            , div
                [ id bodyCollapseId
                , class "collapse show"
                , attribute "aria-labelledby" cardHeaderId
                ]
                [ div
                    []
                    ((flowGraphWithHeader
                        ("flow" ++ (toString i))
                        ( "egal", l )
                     )
                    )
                ]
            ]


collapseButton : String -> Html msg
collapseButton bodyCollapseId =
    a
        [ attribute "data-toggle" "collapse"
        , href <| "#" ++ bodyCollapseId
        , attribute "aria-expanded" "true"
        , attribute "aria-controls" "bodyCollapseId"
        ]
        [ Html.text "V" ]


transformationView : Int -> Transformation String -> List (Html Msg)
transformationView i t =
    Html.div [] [ Html.text <| toString t.transformation ]
        :: case t.transformation of
            CompressColumns ->
                [ h5 [] [ Html.text ("wurst" ++ toString t) ] ]

            DagRenderer.SwapCells _ _ _ ->
                [ h5 [] [ Html.text ("kaese" ++ toString t) ] ]

            DagRenderer.SwapLanes l1 l2 ->
                [ intField i "1st lane " l1 (\l -> (SwapLanes l l2))
                , intField i "2nd lane " l2 (\l -> (SwapLanes l1 l))
                ]

            DagRenderer.Identity ->
                [ div [] [ Html.text ("gouda" ++ toString t) ] ]


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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
